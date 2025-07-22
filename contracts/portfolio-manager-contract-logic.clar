

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
