(module
  invidious.v1
  (
   *fields*
   *host*
   *pretty?*
   *scheme*

   annotations
   captions
   channels
   channels/comments
   channels/latest
   channels/playlists
   channels/search
   channels/videos
   comments
   insights
   mixes
   playlists
   popular
   search
   stats
   suggestions
   top
   trending
   videos
   )

  (import
    scheme
    (only chicken.base
          assert
          compose
          make-parameter
          sub1)
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
  ;;        make an URI
  ;; @param fields A list of fields
  ;; @returns A parameter list, to make an URI
  (define (fields-parm fields)
    (if (null? fields)
        '()
        `((fields . ,(string-join (map ->string fields) ",")))))

  ;; @brief Clean an optional fields parameter
  ;; @param fields #f or a list of strings or symbols
  ;; @param A (possibly empty) listp of strings or symbols
  (define (sanitize-optional-fields-parm fields)
    (assert
      (or (not fields)
          (list? fields))
      "`*fields*` must be `#f` or a (posibly empty) list of strings or symbols")
    (or fields '()))

  ;; @brief Transforms an optional field list into a parameter list,
  ;;        make an URI
  ;; @param fields #f or a list of fields
  ;; @returns A parameter list, to make an URI
  (define (optional-fields-parm fields)
    (fields-parm (sanitize-optional-fields-parm fields)))

  ;; @brief Transform pretty? into a parameter list, make an URI
  ;; @param pretty? #f or non-#f
  ;; @returns '((pretty . 1)) or '()
  (define (pretty?-parm pretty?)
    (if pretty? '((pretty . 1)) '()))

  ;; @brief Filter parameters not given
  ;; @param parms An alist of parameters
  ;; @returns A new alist (possibly empty) with the given parameters
  (: filter-parms ((list-of (pair symbol (or false string))) --> (list-of (pair symbol string))))
  (define (filter-parms parms)
    (filter cdr parms))

  ;; @brief Appends all parameter lists into a single parameter list,
  ;;        to make an URI
  ;; @param parms Other parameters
  ;; @param fields-optional The list of fields
  ;; @param pretty? The pretty? flag
  ;; @returns A parameter list, to make an URI
  (define (combine-parms parms fields-optional pretty?)
    (let ((fields-parm (optional-fields-parm fields-optional))
          (pretty?-parm (pretty?-parm pretty?)))
      (append pretty?-parm fields-parm parms)))

  (define (method-symbol->path sym)
    (string-split (symbol->string sym) "/"))

  (define (positional->path positional)
    (map ->string  positional))

  (define (path method positional)
    `(/ "api" "v1" ,@method ,@positional))

  ;; @brief Makes a complete query URL
  ;; @param method-sym The method-sym request URL, made by concatenating
  ;;     the instance URL, the API path and the method
  ;; @param parms The method specific parameters
  ;; @param fields The list of fields
  ;; @param pretty? The pretty? flag
  ;; @returns A complete query URL
  (define (make-query-url method-sym parms fields pretty? positional)
    (make-uri #:scheme (*scheme*)
              #:path (path (method-symbol->path method-sym)
                           (positional->path positional))
              #:query (combine-parms parms fields pretty?)
              #:host (*host*)))

  ;;;
  ;;; Configuration parameters
  ;;;

  ;; @brief Wrapper for chicken.base.assert
  ;; @param type The expected type
  ;; @param type? Predicate for @a type
  ;; @param var The variable this assert is protecting
  (define (assert* type type? var)
    (lambda (val)
      (assert (type? val) (string-append "`" var "` must be a " type))
      val))

  ;; @brief Pretty-print JSON response? (default is #f)
  ;; @see https://github.com/omarroth/invidious/wiki/API#pretty
  (define *pretty?* (make-parameter #f (compose not not)))

  ;; @brief The fields of the response one is interested in (default is '())
  ;; @see https://github.com/omarroth/invidious/wiki/API#fields
  ;; @see https://developers.google.com/youtube/v3/getting-started#fields
  (define *fields* (make-parameter '() sanitize-optional-fields-parm))

  ;; @brief The scheme to be used (HTTP/S) (default is HTTPS)
  ;; @see uri-common
  (define *scheme* (make-parameter 'https (assert* "symbol" symbol? "*scheme*")))

  ;; @brief The host of the instance to use (default is invidio.us)
  ;; @see https://github.com/omarroth/invidious/wiki/Invidious-Instances
  (define *host* (make-parameter "invidio.us" (assert* "string" string? "*host*")))

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
  ;; @param str The string to append to the API URL
  ;; @param keys The key parameters of the API call
  ;; @param positional One optional (one or none) positional argument
  ;;
  ;; Defines a function named @a name, that takes @a positional positional
  ;; argument and @a keys key arguments, and returns a complete URL to make a
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
       (define (name positional ... #!key (fields (*fields*)) (pretty? (*pretty?*)) (key #f) ...)
         (let ((parms (filter-parms `((key . ,key) ...))))
           (make-query-url 'name parms fields pretty? '(positional ...)))))))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1stats
  (define-iv (stats))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1videosid
  (define-iv (videos id) region)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1annotationsid
  (define-iv (annotations id) source)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1commentsid
  (define-iv (comments id) sort_by source continuation)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1insightsid
  (define-iv (insights))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1captionsid
  (define-iv (captions id) label lang tlang region)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1trending
  (define-iv (trending) type region)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1top
  (define-iv (top))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1popular
  (define-iv (popular))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelsucid
  (define-iv (channels ucid) sort_by)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelsucidvideos-apiv1channelsvideosucid
  (define-iv (channels/videos ucid) page sort_by)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelsucidlatest-apiv1channelslatestucid
  (define-iv (channels/latest))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelsplaylistsucid-apiv1channelsucidplaylists
  (define-iv (channels/playlists ucid) continuation sort_by)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelscommentsucid-apiv1channelsucidcomments
  (define-iv (channels/comments ucid) continuation)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelssearchucid
  (define-iv (channels/search ucid) q page)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1searchsuggestions
  (define-iv (suggestions) q)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1search
  (define-iv (search) q page sort_by date duration type features region)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1playlistsplid
  (define-iv (playlists plid) page)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1mixesrdid
  (define-iv (mixes rdid))
  )
