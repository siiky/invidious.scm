(module
  invidious.v1
  (
   *fields*
   *instance*
   *pretty?*

   api-url
   fields-parms
   optional-fields-parms
   pretty?-parms

   channels
   search
   suggestions
   )

  (import
    scheme
    (only chicken.base
          assert
          make-parameter
          sub1)
    (only chicken.string
          ->string)
    chicken.type)

  (import
    (only srfi-1
          filter
          map)
    (only srfi-13
          string-join)
    (only uri-common
          form-urlencode))

  ;;;
  ;;; Configuration parameters
  ;;;

  ;; @brief Pretty-print response JSON
  ;; @see https://github.com/omarroth/invidious/wiki/API#pretty
  (define *pretty?* (make-parameter #f (lambda (x) (not (not x)))))

  ;; @brief The fields of the response one is interested in
  ;; @see https://github.com/omarroth/invidious/wiki/API#fields
  ;; @see https://developers.google.com/youtube/v3/getting-started#fields
  (define *fields*
    (make-parameter
      #f
      (lambda (df)
        (assert (or (not df)
                    (list? df))
                "`*fields*` must be `#f` or a (posibly empty) list of strigs or symbols")
        (or df '()))))

  ;; @brief The instance to use
  ;; @see https://github.com/omarroth/invidious/wiki/Invidious-Instances
  (define *instance*
    (make-parameter
      "https://invidio.us/"
      (lambda (str)
        (assert (string? str) "`*instance*` must be a string")
        (if (char=? #\/ (string-ref str (sub1 (string-length str))))
            str
            (string-append str "/")))))

  ;;;
  ;;; General & Utility functions
  ;;;

  ;; @brief Transform a list of fields into a parameter list, ready to be
  ;;        passed to form-urlencode
  ;; @param fields A list of fields
  ;; @returns A parameter list, ready to be passed to form-urlencode
  (define (fields-parms fields)
    (if (null? fields)
        '()
        `((fields . ,(string-join (map ->string fields) ",")))))

  ;; @brief Transforms an optional field list into a parameter list, ready to
  ;;        be passed to form-urlencode
  ;; @param fields #f or a list of fields
  ;; @returns A parameter list, ready to be passed to form-urlencode
  (define (optional-fields-parms fields)
    (if (not fields)
        '()
        (fields-parms
          (if (list? fields) fields (*fields*)))))

  ;; @brief Transform pretty? into a parameter list, ready to be passed to
  ;;        form-urlencode
  ;; @param pretty? #f or non-#f
  ;; @returns The singleton list with the '(pretty . 1) pair, or the epty list
  (define (pretty?-parms pretty?)
    (if pretty? '((pretty . 1)) '()))

  ;; @brief Filter parameters not given
  ;; @param parms An alist of parameters
  ;; @returns A new alist (possibly empty) with the given parameters
  (: filter-parms ((list-of (pair symbol (or false string))) --> (list-of (pair symbol string))))
  (define (filter-parms parms)
    (filter cdr parms))

  ;; @brief Appends all parameter lists into a single parameter list, ready to
  ;;        be passed to form-urlencode
  ;; @param parms Other parameters
  ;; @param fields-optional The list of fields
  ;; @param pretty? The pretty? flag
  ;; @returns A parameter list, ready to be passed to form-urlencode
  (define (combine-parms parms fields-optional pretty?)
    (let ((fields-parms (optional-fields-parms fields-optional))
          (pretty?-parms (pretty?-parms pretty?)))
      (append fields-parms pretty?-parms parms)))

  ;; @brief Makes a complete query URL
  ;; @param base The base request URL, made by concatenating the instance URL,
  ;; the API path and the command
  ;; @param parms The command specific parameters
  ;; @param fields The list of fields
  ;; @param pretty? The pretty? flag
  ;; @returns A complete query URL
  (define (make-query-url base parms fields pretty?)
    (let ((final-parms (combine-parms parms fields pretty?)))
      (string-append base "?" (form-urlencode final-parms))))

  ;; @brief Makes the base API URL, optionally with a command
  ;; @param command The command
  ;; @returns The base API URL
  (define (api-url #!optional (command "") (id/ucid ""))
    ; FIXME: command="" & id/ucid="" => "<instance>/api/v1//"
    (string-append (*instance*) "api/v1/" command "/" id/ucid))

  ;;;
  ;;; Command functions
  ;;;
  ;;; Command functions accept the key `fields` and `pretty?` arguments
  ;;; The `fields` key argument can be any value. #f disables the fields
  ;;;     parameter; if given a list, it is used as the list of fields;
  ;;;     defaults to the `*fields*` parameter in all the other cases
  ;;; The `pretty?` key argument enables/disables pretty printing of the
  ;;;     JSON response; defaults to the `*pretty?*` parameter, if not given
  ;;;

  ; TODO: Learn macros. Read https://api.call-cc.org/5/doc/chicken/syntax,
  ;       https://wiki.call-cc.org/man/4/Macros and the rest-bind egg

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelsucid
  (define (channels ucid #!key
                    (sort_by #f)
                    (fields (*fields*))
                    (pretty? (*pretty?*)))
    (let ((base (api-url "channels" ucid))
          (parms (filter-parms `((sort_by . ,sort_by)))))
      (make-query-url base parms fields pretty?)))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1search
  (define (search #!key
                  (date #f)
                  (duration #f)
                  (features #f)
                  (page #f)
                  (q #f)
                  (region #f)
                  (sort_by #f)
                  (type #f)
                  (fields (*fields*))
                  (pretty? (*pretty?*)))
    (let ((base (api-url "search"))
          (parms (filter-parms
                   `((date . ,date)
                     (duration . ,duration)
                     (features . ,features)
                     (page . ,page)
                     (q . ,q)
                     (region . ,region)
                     (sort_by . ,sort_by)
                     (type . ,type)))))
      (make-query-url base parms fields pretty?)))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1searchsuggestions
  (define (suggestions #!key
                       (q #f)
                       (fields (*fields*))
                       (pretty? (*pretty?*)))
    (let ((base (api-url "suggestions"))
          (parms (filter-parms `((q . ,q)))))
      (make-query-url base parms fields pretty?)))
  )
