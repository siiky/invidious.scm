(module
  invidious.uri.v1
  (
   *fields*
   *host*
   *pretty?*
   *scheme*

   instance
   instances
   watch
   )

  (import
    scheme
    (only chicken.base
          assert
          compose
          error
          make-parameter)
    (only chicken.module
          export)
    (only chicken.string
          ->string
          string-split)
    chicken.syntax
    chicken.type)

  (import
    (only srfi-1
          filter
          map)
    (only srfi-13
          string-join)
    (only uri-common
          make-uri))

  ;;;
  ;;; General & Utility functions
  ;;;

  ;; @brief Transform a list of fields into a parameter list, to
  ;;     make an URI
  ;; @param fields A list of fields
  ;; @returns A parameter list, to make an URI
  (: fields-parm ((list-of (or symbol string))
                  --> (or null (list (pair symbol string)))))
  (define (fields-parm fields)
    (if (null? fields)
        '()
        `((fields . ,(string-join (map ->string fields) ",")))))

  ;; @brief Clean an optional fields parameter
  ;; @param fields #f or a list of strings or symbols
  ;; @param A (possibly empty) list of strings or symbols
  (: sanitize-optional-fields-parm ((or false (list-of (or symbol string)))
                                    --> (list-of (or symbol string))))
  (define (sanitize-optional-fields-parm fields)
    (assert
      (or (not fields)
          (list? fields))
      "`*fields*` must be `#f` or a (posibly empty) list of strings or symbols")
    (or fields '()))

  ;; @brief Transforms an optional field list into a parameter list,
  ;;     make an URI
  ;; @param fields #f or a list of fields
  ;; @returns A parameter list, to make an URI
  (: optional-fields-parm ((or false (list-of (or symbol string)))
                           --> (or null (list (pair symbol string)))))
  (define (optional-fields-parm fields)
    (fields-parm (sanitize-optional-fields-parm fields)))

  ;; @brief Transform pretty? into a parameter list, make an URI
  ;; @param pretty? #f or non-#f
  ;; @returns '((pretty . 1)) or '()
  (: pretty?-parm (boolean --> (or (list (pair symbol fixnum)) null)))
  (define (pretty?-parm pretty?)
    (if pretty? '((pretty . 1)) '()))

  ;; @brief Filter parameters not given
  ;; @param parms An alist of parameters
  ;; @returns A new alist (possibly empty) with the given parameters
  (: filter-parms ((list-of (pair symbol (or false string integer)))
                   --> (list-of (pair symbol string))))
  (define (filter-parms parms)
    (filter cdr parms))

  ;; @brief Appends all parameter lists into a single parameter list
  ;; @param parms Other parameters
  ;; @param fields-optional The list of fields
  ;; @param pretty? The pretty? flag
  ;; @returns A parameter list
  (: combine-parms ((list-of (pair symbol string))
                    (list-of string)
                    boolean
                    --> (list-of (pair symbol (or string integer)))))
  (define (combine-parms parms fields-optional pretty?)
    (let ((fields-parm (optional-fields-parm fields-optional))
          (pretty?-parm (pretty?-parm pretty?)))
      (append pretty?-parm fields-parm parms)))

  ;; @brief Convert a method name to a path
  ;; @param sym The symbol representing the method
  ;; @returns A list of strings
  (: method-symbol->path (symbol --> (list-of string)))
  (define (method-symbol->path sym)
    (string-split (symbol->string sym) "/"))

  ;; @brief Convert the positional argument (in a list) to a path
  ;; @param positional The positional argument
  ;; @returns A (possibly empty) list of strings
  (: positional->path ((list-of (or integer symbol string)) --> (list-of string)))
  (define (positional->path positional)
    (map ->string positional))

  ;; @brief Construct a path from the method name and the positional argument
  ;; @param method The symbol representing the method
  ;; @param positional The positional argument
  ;; @returns The path for this method and positional argument
  (: path (symbol string --> (list-of (or symbol string))))
  (define (path method positional)
    `(/ "api" "v1" ,@(method-symbol->path method) ,@(positional->path positional)))

  ;; @brief Makes a complete query URI
  ;; @param method-sym The method-sym request URI, made by concatenating
  ;;     the instance URI, the API path and the method
  ;; @param parms The method specific parameters
  ;; @param fields The list of fields
  ;; @param pretty? The pretty? flag
  ;; @returns A complete query URI
  (define (make-query-url method-sym parms fields pretty? positional)
    (make-uri #:scheme (*scheme*)
              #:path (path method-sym positional)
              #:query (combine-parms parms fields pretty?)
              #:host (*host*)))

  ;;;
  ;;; Configuration parameters
  ;;;

  ;; @brief Wrapper for chicken.base.assert
  ;; @param type The expected type
  ;; @param type? Predicate for @a type
  ;; @param var The variable this assert is protecting
  (: assert* (string (* -> boolean : 'a) string --> (procedure (*) 'a)))
  (define (assert* type type? var)
    (lambda (val)
      (assert (type? val) (string-append "`" var "` must be a " type))
      val))

  ;; @brief Pretty-print JSON response? (default is #f)
  ;; @see https://github.com/iv-org/invidious/wiki/API#pretty
  (: *pretty?* (#!optional * -> boolean))
  (define *pretty?* (make-parameter #f (compose not not)))

  ;; @brief The fields of the response one is interested in (default is '())
  ;; @see https://github.com/iv-org/invidious/wiki/API#fields
  ;; @see https://developers.google.com/youtube/v3/getting-started#fields
  (: *fields* (#!optional (or false (list-of (or symbol string))) -> (list-of (or symbol string))))
  (define *fields* (make-parameter '() sanitize-optional-fields-parm))

  ;; @brief The scheme to be used (HTTP/S) (default is HTTPS)
  ;; @see uri-common
  (: *scheme* (#!optional symbol -> symbol))
  (define *scheme* (make-parameter 'https (assert* "symbol" symbol? "*scheme*")))

  ;; @brief The host of the instance to use (default is invidious.snopyta.org)
  ;; @see https://github.com/iv-org/invidious/wiki/Invidious-Instances
  ;;
  ;; NOTE: invidio.us is no longer active. I've tried a few instances recently,
  ;; and invidious.snopyta.org seems to be pretty stable, is hosted on Finland,
  ;; and is open for non-browser requests -- unlike some other instances :/
  (: *host* (#!optional string -> string))
  (define *host* (make-parameter "invidious.snopyta.org" (assert* "string" string? "*host*")))

  (: instance (--> string))
  (define (instance)
    (string-append (symbol->string (*scheme*)) "://" (*host*)))

  (: watch (string #!key (or 'channel 'video 'playlist) string --> string))
  (define (watch id #!key (type 'video) (instance (instance)))
    (let ((path
            (case type
              ((video) "/watch?v=")
              ((playlist) "/playlist?list=")
              ((channel) "/channel/")
              (else
                (error
                  'watch
                  "Expected 'video, 'playlist or 'channel but got "
                  type)))))
      (string-append instance path id)))

  ;; @brief Compute the URI of the "instances" endpoint on invidious.io.
  ;; @param sort-by Corresponds to the endpoint's `sort_by` query parameter.
  ;;                Defaults to `health`.
  ;; @returns The URI.
  ; TODO: How to specify the URI return type? `(string uri)` compiles, but is
  ;       it right?
  (: instances (#!key string --> (struct uri)))
  (define (instances #!key (sort-by "health"))
    (make-uri #:scheme 'https
              #:path '(/ "instances.json")
              #:query `((sort_by . ,sort-by))
              #:host "api.invidious.io"))

  ;;;
  ;;; Method functions
  ;;;
  ;;; All method functions, in addition to the method specific parameters
  ;;;     specified in the Invidious API documentation, accept the key
  ;;;     `fields` and `pretty?` arguments,
  ;;;
  ;;; The `fields` key argument can be any value. #f disables the fields
  ;;;     parameter; if given a list, it is used as the list of fields;
  ;;;     defaults to the `*fields*` parameter in all the other cases
  ;;;
  ;;; The `pretty?` key argument enables/disables pretty printing of the
  ;;;     JSON response; defaults to the `*pretty?*` parameter, if not given
  ;;;

  ;; @brief Define an Invidious API call
  ;; @param name The name of the functions to define
  ;; @param str The string to append to the API URI
  ;; @param keys The key parameters of the API call
  ;; @param positional One optional (one or none) positional argument
  ;;
  ;; Defines a function named @a name, that takes @a positional positional
  ;; argument and @a keys key arguments, and returns a complete URI to make a
  ;; request.
  ;;
  ;; Example:
  ;; #;> (define-iv (example positional) key1 key2)
  ;;     (example positional #!key (fields (*fields*)) (pretty? (*pretty?*)) (key1 #f) (key2 #f))
  (define-syntax define-iv
    (syntax-rules ()
      ((define-iv (name pos1 pos2 too-many ...) key ...)
       (syntax-error "There must be at most one positional argument"))

      ((define-iv (name positional ...) key ...)
       (begin
         (export name)
         (: name (#!rest (or boolean fixnum string symbol) -> 'uri))
         (define (name positional ... #!key (fields (*fields*)) (pretty? (*pretty?*)) (key #f) ...)
           (let ((parms (filter-parms `((key . ,key) ...))))
             (make-query-url 'name parms fields pretty? `(,positional ...))))))))

  ;; @see https://github.com/iv-org/invidious/wiki/API#get-apiv1stats
  (define-iv (stats))

  ;; @see https://github.com/iv-org/invidious/wiki/API#get-apiv1videosid
  (define-iv (videos id) region)

  ;; @see https://github.com/iv-org/invidious/wiki/API#get-apiv1annotationsid
  (define-iv (annotations id) source)

  ;; @see https://github.com/iv-org/invidious/wiki/API#get-apiv1commentsid
  (define-iv (comments id) sort_by source continuation)

  ;; @see https://github.com/iv-org/invidious/wiki/API#get-apiv1insightsid
  (define-iv (insights))

  ;; @see https://github.com/iv-org/invidious/wiki/API#get-apiv1captionsid
  (define-iv (captions id) label lang tlang region)

  ;; @see https://github.com/iv-org/invidious/wiki/API#get-apiv1trending
  (define-iv (trending) type region)

  ;; @see https://github.com/iv-org/invidious/wiki/API#get-apiv1top
  (define-iv (top))

  ;; @see https://github.com/iv-org/invidious/wiki/API#get-apiv1popular
  (define-iv (popular))

  ;; @see https://github.com/iv-org/invidious/wiki/API#get-apiv1channelsucid
  (define-iv (channels ucid) sort_by)

  ;; @see https://github.com/iv-org/invidious/wiki/API#get-apiv1channelsucidvideos-apiv1channelsvideosucid
  (define-iv (channels/videos ucid) page sort_by)

  ;; @see https://github.com/iv-org/invidious/wiki/API#get-apiv1channelsucidlatest-apiv1channelslatestucid
  (define-iv (channels/latest))

  ;; @see https://github.com/iv-org/invidious/wiki/API#get-apiv1channelsplaylistsucid-apiv1channelsucidplaylists
  (define-iv (channels/playlists ucid) continuation sort_by)

  ;; @see https://github.com/iv-org/invidious/wiki/API#get-apiv1channelscommentsucid-apiv1channelsucidcomments
  (define-iv (channels/comments ucid) continuation)

  ;; @see https://github.com/iv-org/invidious/wiki/API#get-apiv1channelssearchucid
  (define-iv (channels/search ucid) q page)

  ;; @see https://github.com/iv-org/invidious/wiki/API#get-apiv1searchsuggestions
  (define-iv (suggestions) q)

  ;; @see https://github.com/iv-org/invidious/wiki/API#get-apiv1search
  (define-iv (search) q page sort_by date duration type features region)

  ;; @see https://github.com/iv-org/invidious/wiki/API#get-apiv1playlistsplid
  (define-iv (playlists plid) page)

  ;; @see https://github.com/iv-org/invidious/wiki/API#get-apiv1mixesrdid
  (define-iv (mixes rdid))
  )
