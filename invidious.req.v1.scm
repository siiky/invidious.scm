(module
  invidious.req.v1
  (
   sxml-read

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
    (only chicken.eval
          module-environment)
    (only chicken.keyword
          string->keyword)
    (only chicken.string
          ->string
          string-split)
    chicken.syntax
    chicken.type)

  (import
    (only http-client
          with-input-from-request)
    (only json
          json-read)
    (only srfi-1
          concatenate
          filter
          map)
    (only srfi-13
          string-join)
    (only ssax
          ssax:xml->sxml)
    openssl)

  (import
    (only invidious.uri.v1
          *fields*
          *pretty?*))

  (define (symbol->keyword s)
    (string->keyword (symbol->string s)))

  (define (call fun args)
    (apply (eval fun (module-environment 'invidious.uri.v1)) args))

  (define (sxml-read)
    (ssax:xml->sxml (current-input-port) '()))

  (define-syntax define-iv
    (syntax-rules ()
      ((define-iv default-reader (name pos1 pos2 too-many ...) key ...)
       (syntax-error "There must be at most one positional argument"))

      ((define-iv default-reader (name positional ...) key ...)
       (define (name positional ... #!key (reader default-reader) (fields (*fields*)) (pretty? (*pretty?*)) (key #f) ...)
         ; TODO: Is there a better way to do this?
         (let
           ((uri
              (call
                'name
                `(,positional
                   ...
                   #:fields ,fields
                   #:pretty? ,pretty?
                   ,@(append `(,(symbol->keyword 'key) ,key) ...)))))
           (with-input-from-request uri #f reader))))))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1stats
  (define-iv json-read (stats))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1videosid
  (define-iv json-read (videos id) region)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1annotationsid
  (define-iv sxml-read (annotations id) source)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1commentsid
  (define-iv json-read (comments id) sort_by source continuation)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1insightsid
  (define-iv json-read (insights))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1captionsid
  (define-iv json-read (captions id) label lang tlang region)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1trending
  (define-iv json-read (trending) type region)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1top
  (define-iv json-read (top))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1popular
  (define-iv json-read (popular))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelsucid
  (define-iv json-read (channels ucid) sort_by)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelsucidvideos-apiv1channelsvideosucid
  (define-iv json-read (channels/videos ucid) page sort_by)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelsucidlatest-apiv1channelslatestucid
  (define-iv json-read (channels/latest))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelsplaylistsucid-apiv1channelsucidplaylists
  (define-iv json-read (channels/playlists ucid) continuation sort_by)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelscommentsucid-apiv1channelsucidcomments
  (define-iv json-read (channels/comments ucid) continuation)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelssearchucid
  (define-iv json-read (channels/search ucid) q page)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1searchsuggestions
  (define-iv json-read (suggestions) q)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1search
  (define-iv json-read (search) q page sort_by date duration type features region)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1playlistsplid
  (define-iv json-read (playlists plid) page)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1mixesrdid
  (define-iv json-read (mixes rdid))
  )
