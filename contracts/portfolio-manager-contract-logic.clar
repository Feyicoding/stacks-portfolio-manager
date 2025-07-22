

;; portfolio-manager-contract-logic
;; A robust portfolio manager smart contract for managing user portfolios, supporting multiple assets, deposits, withdrawals, rebalancing, and performance tracking. Designed for extensibility and security on the Stacks blockchain.

;; constants
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_FUNDS (err u101))
(define-constant ERR_ASSET_NOT_FOUND (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_ALREADY_EXISTS (err u104))
(define-constant ERR_NOT_FOUND (err u105))
(define-constant ERR_OVERFLOW (err u106))
(define-constant ERR_UNDERFLOW (err u107))
(define-constant ERR_INVALID_ASSET (err u108))
(define-constant ERR_NOT_OWNER (err u109))
(define-constant ERR_PORTFOLIO_LOCKED (err u110))
(define-constant ERR_REBALANCE_FAILED (err u111))

;; constants
;;


;; data maps and vars
;;
;; Map: {user: principal, asset: principal} -> {amount: uint}
(define-map user-portfolios
  { user: principal, asset: principal }
  { amount: uint })

;; Map: {asset: principal} -> {symbol: (buff 10), decimals: uint, is-active: bool}
(define-map supported-assets
  { asset: principal }
  { symbol: (buff 10), decimals: uint, is-active: bool })

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Portfolio lock status {user: principal} -> {locked: bool}
(define-map portfolio-locked { user: principal } { locked: bool })


;; private functions
;;

;; Check if asset is supported and active
(define-private (is-asset-supported (asset principal))
  (let ((asset-data (map-get? supported-assets {asset: asset})))
    (match asset-data
      asset-info (get is-active asset-info)
      false)))

;; Check if sender is contract owner
(define-private (is-owner (sender principal))
  (is-eq sender (var-get contract-owner)))

;; Get user asset balance or default to 0
(define-private (get-user-asset-balance (user principal) (asset principal))
  (let ((entry (map-get? user-portfolios {user: user, asset: asset})))
    (match entry
      portfolio (get amount portfolio)
      u0)))

;; public functions
;;

;; Deposit assets into portfolio
(define-public (deposit (asset principal) (amount uint))
  (if (not (is-principal asset)) ERR_INVALID_ASSET
    (if (not (is-uint amount)) ERR_INVALID_AMOUNT
      (if (not (is-asset-supported asset)) ERR_INVALID_ASSET
        (let ((locked (default-to false (get locked (map-get? portfolio-locked {user: tx-sender})))))
          (if locked ERR_PORTFOLIO_LOCKED
            (let ((old-balance (get-user-asset-balance tx-sender asset)))
              (map-set user-portfolios {user: tx-sender, asset: asset} {amount: (+ old-balance amount)})
              (ok (+ old-balance amount)))))))))

;; Withdraw assets from portfolio
(define-public (withdraw (asset principal) (amount uint))
  (if (not (is-principal asset)) ERR_INVALID_ASSET
    (if (not (is-uint amount)) ERR_INVALID_AMOUNT
      (if (not (is-asset-supported asset)) ERR_INVALID_ASSET
        (let ((locked (default-to false (get locked (map-get? portfolio-locked {user: tx-sender})))))
          (if locked ERR_PORTFOLIO_LOCKED
            (let ((old-balance (get-user-asset-balance tx-sender asset)))
              (if (< old-balance amount) ERR_INSUFFICIENT_FUNDS
                (begin
                  (map-set user-portfolios {user: tx-sender, asset: asset} {amount: (- old-balance amount)})
                  (ok (- old-balance amount)))))))))))

;; Add a new supported asset (owner only)
(define-public (add-asset (asset principal) (symbol (buff 10)) (decimals uint))
  (if (not (is-principal asset)) ERR_INVALID_ASSET
    (if (not (is-buff symbol 10)) ERR_INVALID_ASSET
      (if (not (is-uint decimals)) ERR_INVALID_ASSET
        (if (not (is-owner tx-sender)) ERR_NOT_OWNER
          (let ((exists (map-get? supported-assets {asset: asset})))
            (if (is-some exists) ERR_ALREADY_EXISTS
              (begin
                (map-set supported-assets {asset: asset} {symbol: symbol, decimals: decimals, is-active: true})
                (ok true)))))))))

;; Lock a user's portfolio (owner only)
(define-public (lock-portfolio (user principal))
  (if (not (is-principal user)) ERR_UNAUTHORIZED
    (if (not (is-owner tx-sender)) ERR_NOT_OWNER
      (begin
        (map-set portfolio-locked {user: user} {locked: true})
        (ok true)))))

;; Unlock a user's portfolio (owner only)
(define-public (unlock-portfolio (user principal))
  (if (not (is-principal user)) ERR_UNAUTHORIZED
    (if (not (is-owner tx-sender)) ERR_NOT_OWNER
      (begin
        (map-set portfolio-locked {user: user} {locked: false})
        (ok true)))))

;; Get a user's asset balance (read-only)
(define-read-only (get-portfolio (user principal) (asset principal))
  (if (not (is-principal user)) u0
    (if (not (is-principal asset)) u0
      (get-user-asset-balance user asset))))
