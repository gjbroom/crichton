;;;; package.lisp
;;;;
;;;; Package definitions for Crichton

(defpackage #:crichton/config
  (:use #:cl)
  (:export #:*agent-home*
           #:*config*
           #:*crichton-version*
           #:agent-path
           #:ensure-directories
           #:load-config
           #:reload-config
           #:config-get
           #:config-section-get
           #:config-section-get-keyword
           ;; TOML conversion (config/loader.lisp)
           #:toml-key-to-keyword
           #:toml-value-to-lisp
           #:toml-table-to-plist
           #:deep-merge-plist
           #:plistp
           ;; shared utilities (config/util.lisp)
           #:iso8601-now
           #:safe-intern-keyword
           #:plist-to-json-bytes
           #:json-bytes-to-plist
           #:delete-file-if-exists
           #:string-case))

(defpackage #:crichton/storage
  (:use #:cl)
  (:local-nicknames (#:bt #:bordeaux-threads))
  (:export #:store-get
           #:store-set
           #:store-delete
           #:store-exists-p
           #:store-list-keys
           #:store-list-namespaces
           #:store-get-all
           #:store-set-all
           #:store-clear-namespace
           #:flush-namespace
           #:flush-all-storage
           #:preload-storage
           #:clear-storage-cache
           #:storage-usage
           #:storage-report))

(defpackage #:crichton/logging
  (:use #:cl)
  (:export #:setup-logging
           #:suppress-console-logging
           #:with-redaction
           #:write-audit-event
           #:redact-channel-output))

(defpackage #:crichton/daemon
  (:use #:cl)
  (:local-nicknames (#:bt #:bordeaux-threads))
  (:export #:start-daemon
           #:stop-daemon
           #:daemon-status
           #:start-swank
           #:ensure-runner
           #:runner-invoke
           #:kill-runner
           #:*running*
           #:*shutdown-lock*
           #:*shutdown-cv*
           #:start-rpc-server
           #:stop-rpc-server
           #:daemon-socket-path
           #:add-subscriber
           #:remove-subscriber
           #:notification-post))

(defpackage #:crichton/rpc
  (:use #:cl)
  (:export #:*max-message-bytes*
           #:next-id
           #:bytes-to-base64
           #:base64-to-bytes
           #:json-to-string
           #:string-to-json
           #:make-request
           #:make-ok-response
           #:make-error-response
           #:write-message
           #:read-message
           #:msg-id
           #:msg-op
           #:msg-ok-p
           #:msg-result
           #:msg-error
           #:msg-get))

(defpackage #:crichton/wasm
  (:use #:cl)
  (:export #:ensure-wasmtime-loaded
           #:assert-wasmtime-abi
           #:call-with-wasm-bytes
           #:call-with-compiled-module
           #:call-with-instantiated-instance
           #:call-with-wasm-environment
           #:run-wasm-module
           #:run-wasm-with-host-fns
           #:run-wasm-bytes-with-host-fns
           #:run-wasm-json-call
           #:run-wasm-bytes-json-call
           #:call-export-by-name
           #:wat-to-wasm
           #:make-functype
           #:*test-host-log-wat*))

(defpackage #:crichton/skills
  (:use #:cl)
  (:local-nicknames (#:bt #:bordeaux-threads))
  (:export #:parse-skill-manifest
           #:validate-capabilities
           #:manifest-skill-name
           #:manifest-skill-version
           #:manifest-capabilities
           #:manifest-function-info
           #:manifest-function-abi
           #:manifest-function-input-type
           #:manifest-function-output-type
           #:manifest-function-description
           #:manifest-error
           #:manifest-error-path
           #:manifest-error-problems
           #:generate-signing-keypair
           #:sign-skill-bytes
           #:verify-skill-signature
           #:load-trusted-keys
           #:verify-skill-bundle
           ;; skill registry
           #:skill-entry
           #:discover-skills
           #:list-skills
           #:skill-info
           #:load-skill
           #:invoke-skill
           #:unload-skill
           #:*max-skill-json-input-bytes*
           #:skill-report
           ;; pipeline executor
           #:execute-pipeline
           #:pipeline-error
           #:pipeline-error-step-id
           #:pipeline-error-step-skill
           #:register-pipeline-builtin
           #:register-default-pipeline-builtins
           ;; saved pipelines
           #:save-pipeline
           #:delete-pipeline
           #:list-saved-pipelines
           #:get-saved-pipeline
           #:restore-saved-pipelines
           #:weather-report
           #:weather-conditions
           #:system-loadavg
           #:system-memory
           #:system-thermal-zones
           #:system-disk-usage
           #:system-snapshot
           #:system-report
           #:system-monitor-config
           #:start-system-monitoring
           #:stop-system-monitoring
           #:restore-system-monitoring
           ;; timing
           #:with-timing
           #:make-stopwatch
           #:stopwatch-record
           #:stopwatch-reset
           #:stopwatch-snapshot
           #:with-stopwatch
           #:get-stopwatch
           #:list-stopwatches
           #:clear-stopwatches
           #:format-duration-us
           ;; scheduler
           #:start-scheduler
           #:stop-scheduler
           #:schedule-at
           #:schedule-every
           #:schedule-daily
           #:schedule-prompt-at
           #:schedule-prompt-every
           #:schedule-prompt-daily
           #:cancel-task
           #:list-tasks
           #:scheduler-status
           #:register-schedulable-action
           #:get-schedulable-action
           #:list-schedulable-actions
           ;; task persistence
           #:user-task-p
           #:persist-user-tasks
           #:restore-user-tasks
           #:save-user-tasks
           #:load-user-tasks
           #:list-unrestorable-tasks
           #:export-user-tasks
           ;; current time
           #:current-time-plist
           #:current-time-report
           ;; ephemeris (sunrise/sunset/solar data + lunar phases)
           #:ephemeris-plist
           #:current-ephemeris
           #:ephemeris-report
           #:lunar-phase-for-date
           #:lunar-phase-description
           ;; rss
           #:rss-fetch
           #:rss-check
           #:rss-report
           #:rss-check-report
           #:rss-monitor-start
           #:rss-monitor-stop
           #:rss-monitor-mute
           #:rss-monitor-unmute
           #:rss-reset-all-backoff
           #:rss-monitor-configs
           #:rss-list-monitors
           #:opml-import-monitors
           #:opml-export-monitors
           #:restore-rss-monitors
           #:run-rss-filter
           ;; rss article inbox
           #:inbox-insert-articles
           #:inbox-get-unscored
           #:inbox-get-unsaved
           #:inbox-by-feed
           #:inbox-saved-since
           #:inbox-save-scores
           #:inbox-mark-saved
           #:inbox-mark-reviewed
           #:inbox-stats
           ;; rss curation (scorer, briefing, query)
           #:get-interests-profile
           #:set-interests-profile
           #:run-interest-scorer
           #:rss-morning-briefing
           #:rss-inbox-query
           #:format-inbox-query-result
           #:start-rss-curator
           #:restore-rss-curator
           #:clear-seen
           #:persist-rss-state
           #:load-rss-state
           #:clear-all-rss-state
           #:rss-state-stats
           #:enable-rss-persistence
           #:disable-rss-persistence
           ;; rss feed writing/generation
           #:rss-feed-configure
           #:rss-feed-publish
           #:rss-feed-items
           #:rss-feed-xml
           #:rss-feed-clear
           #:rss-feed-delete
           #:rss-feed-list
           ;; token-usage / metered resources
           #:register-pricing
           #:record-usage
           #:ensure-meter
           #:list-meters
           #:reset-meter
           #:reset-all-meters
           #:meter-snapshot
           #:all-meters-snapshot
           #:meter-recent
           #:aggregate-snapshot
           #:meter-report
           #:usage-report
           #:save-meters
           #:load-meters
           #:persist-meter
           #:enable-meter-persistence
           #:disable-meter-persistence
           ;; battery monitoring
           #:has-battery-p
           #:list-batteries
           #:battery-snapshot
           #:all-batteries-snapshot
           #:battery-status-plist
           #:battery-report
           #:battery-check-thresholds
           #:start-battery-monitoring
           #:stop-battery-monitoring
           #:restore-battery-monitoring
           ;; log inspector
           #:log-file-path
           #:read-log-tail
           #:search-log
           #:log-summary
           #:log-report
           ;; kv-store
           #:kv-get
           #:kv-set
           #:kv-delete
           #:kv-exists-p
           #:kv-list
           #:kv-clear-skill
           #:kv-skill-usage
           #:kv-global-usage
           #:preload-kv-cache
           #:flush-all-kv
           #:clear-kv-cache
           #:kv-usage-report
           #:kv-quota-exceeded
           ;; kv-store operational tools
           #:kv-dir-path
           #:backup-kv-store
           #:restore-kv-backup
           #:check-kv-integrity
           #:repair-corrupt-kv
           #:kv-health-check
           ;; amp orchestrator
           #:amp-available-p
           #:amp-enabled-p
           #:amp-status
           #:amp-invoke
           #:amp-code-task
           #:amp-test-task
           #:amp-report
           ;; raindrop.io bookmarks
           #:raindrop-save
           #:raindrop-find
           #:format-raindrop
           #:format-raindrop-list
           #:raindrop-get-one
           #:raindrop-update
           #:raindrop-remove
           #:raindrop-list
           #:raindrop-search
           #:raindrop-list-collections
           #:raindrop-list-tags
           #:raindrop-create-collection
           #:raindrop-save-report
           #:raindrop-find-report
           #:raindrop-collections-report
           #:raindrop-tags-report
           ;; books
           #:books-enabled-p
           #:books-db-path
           #:books-search
           #:books-by-tag
           #:books-by-author
           #:books-by-collection
           #:books-by-series
           #:books-get
           #:books-list-tags
           #:books-list-collections
           #:books-list-series
           #:books-status
           ;; orgmode
           #:orgmode-enabled-p
           #:orgmode-status
           #:orgmode-read
           #:orgmode-read-file
           #:orgmode-search
           #:orgmode-list-tags
           #:orgmode-backlinks
           #:orgmode-create-note
           #:orgmode-append
           #:orgmode-list-files
           #:orgmode-list-todos
           #:orgmode-set-todo
           #:orgmode-set-filetags
           ;; pushover
           #:pushover-send
           #:pushover-report
           ;; github
           #:github-list-repos
           #:github-get-repo
           #:github-list-issues
           #:github-create-issue
           #:github-list-prs
           #:github-list-workflow-runs
           #:github-list-releases
           #:github-search-code
           #:github-report
           ;; hoobs
           #:hoobs-accessories
           #:hoobs-get-accessory
           #:hoobs-set-accessory
           #:hoobs-rooms
           #:hoobs-service-status
           #:hoobs-report
           ;; git
           #:git-enabled-p
           #:git-config-status
           #:git-status
           #:git-log
           #:git-diff
           #:git-branches
           #:git-worktrees
           #:git-read-file
           #:git-show
           #:git-write-file
           #:git-stage
           #:git-unstage
           #:git-commit
           #:git-create-branch
           #:git-checkout
           ;; retry infrastructure
           #:operation-cancelled
           #:with-retry
           #:with-timeout
           #:http-get-with-retry
           #:http-post-with-retry
           #:http-put-with-retry
           #:http-delete-with-retry
           #:get-retry-config
           #:transient-error-p
           #:rate-limiter
           #:check-rate-limit
           #:cleanup-rate-limiter
           #:*default-max-retries*
           #:*default-backoff-base*
           #:*default-max-backoff*))

(defpackage #:crichton/llm
  (:use #:cl)
  (:local-nicknames (#:bt #:bordeaux-threads))
  (:export ;; protocol
           #:llm-provider
           #:provider-id
           #:provider-model
           #:send-message
           #:stream-message
           #:list-models
           #:normalize-messages
           #:extract-system-message
           #:response-text
           ;; content blocks
           #:content-blocks
           #:blocks-text
           #:blocks-tool-uses
           #:make-tool-result-block
           ;; conditions
           #:llm-error
           #:llm-error-provider
           #:llm-error-message
           #:llm-api-error
           #:llm-api-error-status
           #:llm-api-error-body
           #:llm-auth-error
           #:llm-rate-limit-error
           #:llm-rate-limit-retry-after
           #:llm-feature-not-supported
           ;; anthropic serialization (pure functions, also useful for tests)
           #:content-block-to-anthropic
           #:messages-to-anthropic
           #:parse-anthropic-response
           #:tool-choice-to-anthropic
           #:build-anthropic-request
           ;; anthropic
           #:anthropic-provider
           #:make-anthropic-provider
           ;; registry
           #:*llm-provider*
           #:make-llm-provider-from-config
           #:ensure-llm-provider
           #:chat))

(defpackage #:crichton/agent
  (:use #:cl)
  (:export #:run-agent
           #:run-agent/stream
           #:ask
           #:chat-session
           #:register-all-tools
           #:all-tool-defs
           #:channel-safe-tool-defs
           #:dispatch-tool
           #:register-tool
           #:sanitize-user-input
           #:strip-control-chars
           #:count-injection-markers
           #:hget
           #:make-json-schema
           #:symbol-to-underscored
           #:+max-channel-input-length+
           #:+max-direct-input-length+))

(defpackage #:crichton/crypto
  (:use #:cl)
  (:export #:age-available-p
           #:ensure-identity-key
           #:identity-key-exists-p
           #:identity-key-path
           #:identity-recipient
           #:age-encrypt
           #:age-decrypt
           #:encrypt-string
           #:decrypt-to-string
           #:encrypt-to-file
           #:decrypt-from-file
           #:read-file-bytes
           #:wipe-bytes
           #:wipe-string
           #:with-secret-bytes
           #:with-secret-string))

(defpackage #:crichton/credentials
  (:use #:cl)
  (:export #:credential-store
           #:credential-not-found
           #:credential-name
           #:credential-backend-error
           #:cred-put
           #:cred-get
           #:cred-delete
           #:cred-exists-p
           #:cred-list
           #:age-file-store
           #:make-age-file-store
           #:*credential-store*
           #:ensure-credential-store
           #:resolve-credential
           #:resolve-credential-for-skill
           #:store-credential
           #:delete-credential
           #:list-credentials))

(defpackage #:crichton/state
  (:use #:cl)
  (:export #:state-dir
           #:load-bootstrap-files
           #:filter-bootstrap-for-session
           #:bootstrap-system-prompt
           #:ensure-default-state-files
           ;; journal & memory (state/journal.lisp)
           #:journal-dir
           #:journal-today-path
           #:journal-append
           #:journal-search
           #:append-to-memory
           ;; pre-compaction (state/journal.lisp)
           #:estimate-messages-tokens
           #:flush-session-to-journal))

(defpackage #:crichton/sessions
  (:use #:cl)
  (:export #:create-session
           #:save-session
           #:load-session
           #:list-sessions
           #:delete-session
           #:add-message
           #:purge-expired-sessions))

(defpackage #:crichton/channels
  (:use #:cl)
  (:local-nicknames (#:bt #:bordeaux-threads))
  (:export #:channel-connect
           #:channel-disconnect
           #:channel-send
           #:channel-set-handler
           #:channel-name
           #:channel-message
           #:make-channel-message
           #:channel-message-id
           #:channel-message-text
           #:channel-message-author-id
           #:channel-message-author-name
           #:channel-message-channel-id
           #:channel-message-guild-id
           #:channel-message-raw
           #:start-channels
           #:stop-channels))

(defpackage #:crichton/channels/discord
  (:use #:cl)
  (:local-nicknames (#:bt #:bordeaux-threads)
                    (#:wsd #:websocket-driver))
  (:export #:discord-channel
           #:make-discord-channel))

(defpackage #:crichton/runner
  (:use #:cl)
  (:export #:main
           ;; skill context
           #:skill-context
           #:make-skill-context-from-manifest
           #:call-with-skill-context
           #:current-skill-context
           #:skill-context-id
           #:skill-http-allowlist
           #:skill-kv-store
           #:skill-resolve-secret))


