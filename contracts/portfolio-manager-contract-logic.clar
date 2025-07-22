

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
;; Map: user principal -> portfolio (map of asset -> amount)
(define-map user-portfolios
  ((user principal))
  (assets (list 20 (tuple (asset principal) (amount uint)))))

;; Map: asset principal -> asset metadata
(define-map supported-assets
  ((asset principal))
  ((symbol (buff 10)) (decimals uint) (is-active bool)))

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Portfolio lock status (user principal -> bool)
(define-map portfolio-locked ((user principal)) ((locked bool)))


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

;; Get user portfolio or default
(define-private (get-user-portfolio (user principal))
  (default-to {assets: (list)} (map-get? user-portfolios {user: user})))

;; public functions
;;
