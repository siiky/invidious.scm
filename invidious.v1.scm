(module
  invidious.v1
  (
   *fields*
   *instance*
   *pretty?*

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
          ->string)
    chicken.syntax
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
  ;;; General & Utility functions
  ;;;

  ;; @brief Transform a list of fields into a parameter list, ready to be
  ;;        passed to form-urlencode
  ;; @param fields A list of fields
  ;; @returns A parameter list, ready to be passed to form-urlencode
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

  ;; @brief Transforms an optional field list into a parameter list, ready to
  ;;        be passed to form-urlencode
  ;; @param fields #f or a list of fields
  ;; @returns A parameter list, ready to be passed to form-urlencode
  (define (optional-fields-parm fields)
    (fields-parm (sanitize-optional-fields-parm fields)))

  ;; @brief Transform pretty? into a parameter list, ready to be passed to
  ;;        form-urlencode
  ;; @param pretty? #f or non-#f
  ;; @returns The singleton list with the '(pretty . 1) pair, or the epty list
  (define (pretty?-parm pretty?)
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
    (let ((fields-parm (optional-fields-parm fields-optional))
          (pretty?-parm (pretty?-parm pretty?)))
      (append pretty?-parm fields-parm parms)))

  ;; @brief Makes a complete query URL
  ;; @param base The base request URL, made by concatenating the instance URL,
  ;; the API path and the command
  ;; @param parms The command specific parameters
  ;; @param fields The list of fields
  ;; @param pretty? The pretty? flag
  ;; @returns A complete query URL
  (define (make-query-url base parms fields pretty?)
    (let ((parms (combine-parms parms fields pretty?)))
      (string-append base "?" (or (form-urlencode parms) ""))))

  ;; @brief Makes the base API URL, optionally with a command
  ;; @param command The command
  ;; @returns The base API URL
  (define (api-url #!optional (command "") (id/ucid ""))
    ; FIXME: command="" & id/ucid="" => "<instance>/api/v1//"
    (string-append (*instance*) "api/v1/" command "/" (->string id/ucid)))

  ;;;
  ;;; Configuration parameters
  ;;;

  ;; @brief Pretty-print JSON response?
  ;; @see https://github.com/omarroth/invidious/wiki/API#pretty
  (define *pretty?* (make-parameter #f (compose not not)))

  ;; @brief The fields of the response one is interested in
  ;; @see https://github.com/omarroth/invidious/wiki/API#fields
  ;; @see https://developers.google.com/youtube/v3/getting-started#fields
  (define *fields* (make-parameter '() sanitize-optional-fields-parm))

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
  ;;; Command functions
  ;;;
  ;;; Command functions accept the key `fields` and `pretty?` arguments
  ;;; The `fields` key argument can be any value. #f disables the fields
  ;;;     parameter; if given a list, it is used as the list of fields;
  ;;;     defaults to the `*fields*` parameter in all the other cases
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
      ((define-iv str (name pos1 pos2 too-many ...) key ...)
       (syntax-error "There must be at most one positional argument"))
      ((define-iv str (name positional ...) key ...)
       (define (name positional ... #!key (fields (*fields*)) (pretty? (*pretty?*)) (key #f) ...)
         (let ((base (api-url str positional ...))
               (parms (filter-parms `((key . ,key) ...))))
           (make-query-url base parms fields pretty?))))))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1stats
  (define-iv "stats" (stats))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1videosid
  (define-iv "videos" (videos id) region)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1annotationsid
  (define-iv "annotations" (annotations id) source)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1commentsid
  (define-iv "comments" (comments id) sort_by source continuation)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1insightsid
  (define-iv "insights" (insights))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1captionsid
  (define-iv "captions" (captions id) label lang tlang region)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1trending
  (define-iv "trending" (trending) type region)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1top
  (define-iv "top" (top))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1popular
  (define-iv "popular" (popular))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelsucid
  (define-iv "channels" (channels ucid) sort_by)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelsucidvideos-apiv1channelsvideosucid
  (define-iv "channels/videos" (channels/videos ucid) page sort_by)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelsucidlatest-apiv1channelslatestucid
  (define-iv "channels/latest" (channels/latest))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelsplaylistsucid-apiv1channelsucidplaylists
  (define-iv "channels/playlists" (channels/playlists ucid) continuation sort_by)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelscommentsucid-apiv1channelsucidcomments
  (define-iv "channels/comments" (channels/comments ucid) continuation)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelssearchucid
  (define-iv "channels/search" (channels/search ucid) q page)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1searchsuggestions
  (define-iv "suggestions" (suggestions) q)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1search
  (define-iv "search" (search) q page sort_by date duration type features region)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1playlistsplid
  (define-iv "playlists" (playlists plid) page)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1mixesrdid
  (define-iv "mixes" (mixes rdid))
  )
