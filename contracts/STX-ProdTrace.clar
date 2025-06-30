
;; STX-ProdTrace
;; title: source
;; version:
;; summary:
;; description:
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-ROLE (err u101))
(define-constant ERR-NOT-FOUND (err u102))
(define-constant ERR-INVALID-INPUT (err u103))
(define-constant ERR-ALREADY-EXISTS (err u104))

;; traits
;;
(define-data-var contract-owner principal tx-sender)

;; token definitions
;;
(define-constant MANUFACTURER u"manufacturer")
(define-constant TRANSPORTER u"transporter")
(define-constant RETAILER u"retailer")
(define-constant MIN-STRING-LENGTH u1)
(define-constant MAX-STRING-LENGTH-100 u100)
(define-constant MAX-STRING-LENGTH-50 u50)

;; constants
;;
(define-map roles principal 
  {
    role: (string-utf8 20),
    is-active: bool
  }
)

;; data vars
;;
(define-map products uint 
  { 
    id: uint,
    name: (string-utf8 100),
    manufacturer: principal,
    origin: (string-utf8 50),
    timestamp: uint,
    current-location: (string-utf8 100),
    status: (string-utf8 20)
  }
)

;; data maps
;;
;; New map for product history
(define-map product-history 
  {product-id: uint, change-id: uint} 
  { 
    timestamp: uint,
    location: (string-utf8 100),
    status: (string-utf8 20)
  }
)

;; public functions
;;
(define-data-var product-counter uint u0)
(define-data-var change-counter uint u0) ;; To track the number of changes for history

;; read only functions
;;
(define-private (is-valid-role (role (string-utf8 20)))
  (or 
    (is-eq role MANUFACTURER)
    (is-eq role TRANSPORTER)
    (is-eq role RETAILER)
  )
)

;; private functions
;;
(define-private (is-valid-string-length (str (string-utf8 100)) (max-len uint))
  (and 
    (>= (len str) MIN-STRING-LENGTH)
    (<= (len str) max-len)
  )
)

(define-private (validate-strings 
    (name (string-utf8 100))
    (origin (string-utf8 50))
    (location (string-utf8 100)))
  (and 
    (is-valid-string-length name MAX-STRING-LENGTH-100)
    (is-valid-string-length origin MAX-STRING-LENGTH-50)
    (is-valid-string-length location MAX-STRING-LENGTH-100)
  )
)

(define-private (is-contract-owner)
  (is-eq tx-sender (var-get contract-owner))
)

(define-private (check-role (address principal) (required-role (string-utf8 20)))
  (match (map-get? roles address)
    role (and 
          (is-eq (get role role) required-role)
          (get is-active role))
    false
  )
)
