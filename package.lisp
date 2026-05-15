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

(defpackage #:crichton/skills
  (:use #:cl)
  (:local-nicknames (#:bt #:bordeaux-threads))
  (:export ;; skill manifest / signing (used by runner and external tools)
           #:parse-skill-manifest
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
           ;; skill registry (framework, not domain tools)
           #:skill-entry
           #:list-skills
           #:load-skill
           #:unload-skill
           #:*max-skill-json-input-bytes*
           ;; pipeline framework (extension points)
           #:pipeline-error-step-skill
           #:register-pipeline-builtin
           #:get-saved-pipeline
           ;; ephemeris primitives
           #:ephemeris-plist
           #:current-ephemeris
           #:lunar-phase-for-date
           #:lunar-phase-description
           ;; rss low-level (used directly by rss subsystem)
           #:rss-fetch
           #:rss-check
           #:rss-list-monitors
           #:run-rss-filter
           ;; rss curation engine
           #:run-interest-scorer
           #:rss-morning-briefing
           #:format-inbox-query-result
           #:start-rss-curator
           #:persist-rss-state
           ;; task persistence internals (serialization layer)
           #:user-task-p
           #:save-user-tasks
           #:load-user-tasks
           #:export-user-tasks
           ;; scheduler extension point
           #:register-schedulable-action
           ;; token-usage metering (used by llm/protocol.lisp)
           #:register-pricing
           #:record-usage
           ;; log inspector (path/summary only; report fn is domain)
           #:log-file-path
           #:log-summary
           ;; kv-store (public API for skill implementations)
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
           #:kv-dir-path
           #:backup-kv-store
           #:restore-kv-backup
           #:check-kv-integrity
           #:repair-corrupt-kv
           #:kv-health-check
           ;; amp orchestrator (control plane, not domain report)
           #:amp-available-p
           #:amp-enabled-p
           #:amp-invoke
           ;; raindrop low-level (primitive operations)
           #:raindrop-save
           #:raindrop-find
           #:raindrop-search
           #:raindrop-list-collections
           #:raindrop-list-tags
           ;; provider availability checks
           #:books-enabled-p
           #:books-db-path
           #:orgmode-enabled-p
           #:git-enabled-p
           ;; pushover primitive
           #:pushover-send
           ;; github low-level (primitive operations)
           #:github-list-repos
           #:github-get-repo
           #:github-list-issues
           #:github-list-prs
           #:github-list-workflow-runs
           #:github-list-releases
           #:github-search-code
           ;; hoobs low-level
           #:hoobs-accessories
           #:hoobs-service-status
           ;; retry / cancellation (shared with llm)
           #:operation-cancelled
           #:with-retry)
  (:intern ;; domain tool symbols — internal to crichton/skills, imported explicitly
           ;; by crichton/agent and crichton/daemon via (:import-from ...)
           ;; rate limiting (daemon)
           #:rate-limiter
           #:check-rate-limit
           ;; interests profile (daemon)
           #:get-interests-profile
           #:set-interests-profile
           ;; rss inbox query (daemon)
           #:rss-inbox-query
           ;; meters (daemon+agent)
           #:load-meters
           #:save-meters
           #:list-meters
           #:meter-recent
           #:meter-report
           #:usage-report
           ;; scheduler control (daemon+agent)
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
           #:get-schedulable-action
           #:list-schedulable-actions
           ;; task persistence (daemon+agent)
           #:persist-user-tasks
           #:restore-user-tasks
           #:list-unrestorable-tasks
           ;; restore / startup hooks (daemon)
           #:restore-saved-pipelines
           #:restore-rss-monitors
           #:restore-rss-curator
           #:restore-system-monitoring
           #:restore-battery-monitoring
           ;; monitoring control (daemon+agent)
           #:weather-report
           #:system-report
           #:system-monitor-config
           #:start-system-monitoring
           #:stop-system-monitoring
           #:battery-report
           #:start-battery-monitoring
           #:stop-battery-monitoring
           ;; time / ephemeris (agent)
           #:current-time-plist
           #:current-time-report
           #:ephemeris-report
           ;; rss monitors (agent)
           #:rss-report
           #:rss-check-report
           #:rss-monitor-start
           #:rss-monitor-stop
           #:rss-monitor-stop-many
           #:rss-monitor-mute
           #:rss-monitor-unmute
           #:rss-reset-all-backoff
           #:rss-monitor-configs
           #:opml-import-monitors
           #:opml-export-monitors
           ;; rss feeds (agent)
           #:rss-feed-configure
           #:rss-feed-publish
           #:rss-feed-items
           #:rss-feed-xml
           #:rss-feed-clear
           #:rss-feed-delete
           #:rss-feed-list
           ;; log inspector (agent)
           #:read-log-tail
           #:search-log
           #:log-report
           ;; amp domain tools (agent)
           #:amp-status
           #:amp-code-task
           #:amp-test-task
           #:amp-report
           ;; raindrop domain tools (agent)
           #:format-raindrop
           #:format-raindrop-list
           #:raindrop-get-one
           #:raindrop-update
           #:raindrop-remove
           #:raindrop-list
           #:raindrop-create-collection
           #:raindrop-save-report
           #:raindrop-find-report
           #:raindrop-collections-report
           #:raindrop-tags-report
           ;; books domain tools (agent)
           #:books-search
           #:books-by-tag
           #:books-by-tags
           #:books-by-author
           #:books-by-collection
           #:books-by-series
           #:books-get
           #:books-list-tags
           #:books-list-collections
           #:books-list-series
           #:books-status
           ;; orgmode domain tools (agent)
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
           ;; pushover domain tool (agent)
           #:pushover-report
           ;; github domain tools (agent)
           #:github-create-issue
           #:github-report
           ;; hoobs domain tools (agent)
           #:hoobs-get-accessory
           #:hoobs-set-accessory
           #:hoobs-rooms
           #:hoobs-report
           ;; git domain tools (agent)
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
           ;; retry / timeout (agent)
           #:with-timeout
           ;; skills / pipeline (agent+daemon)
           #:discover-skills
           #:skill-info
           #:invoke-skill
           #:skill-report
           #:execute-pipeline
           #:pipeline-error
           #:pipeline-error-step-id
           #:register-default-pipeline-builtins
           #:save-pipeline
           #:delete-pipeline
           #:list-saved-pipelines))

(defpackage #:crichton/daemon
  (:use #:cl)
  (:local-nicknames (#:bt #:bordeaux-threads))
  (:import-from #:crichton/skills
                ;; rate limiting
                #:rate-limiter
                #:check-rate-limit
                ;; kv / storage
                #:preload-kv-cache
                #:flush-all-kv
                ;; meters
                #:load-meters
                #:save-meters
                ;; interests profile
                #:get-interests-profile
                #:set-interests-profile
                ;; scheduler
                #:start-scheduler
                #:stop-scheduler
                #:schedule-every
                ;; task persistence
                #:persist-user-tasks
                #:restore-user-tasks
                ;; restore / startup hooks
                #:restore-saved-pipelines
                #:restore-rss-monitors
                #:restore-rss-curator
                #:restore-system-monitoring
                #:restore-battery-monitoring
                ;; monitoring control
                #:start-battery-monitoring
                #:stop-battery-monitoring
                #:stop-system-monitoring
                ;; discovery and rss
                #:discover-skills
                #:rss-inbox-query)
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
  (:import-from #:crichton/skills
                ;; skill registry / pipeline
                #:discover-skills
                #:skill-info
                #:invoke-skill
                #:skill-report
                #:execute-pipeline
                #:pipeline-error
                #:pipeline-error-step-id
                #:register-default-pipeline-builtins
                #:save-pipeline
                #:delete-pipeline
                #:list-saved-pipelines
                ;; scheduler
                #:start-scheduler
                #:schedule-at
                #:schedule-every
                #:schedule-daily
                #:schedule-prompt-at
                #:schedule-prompt-every
                #:schedule-prompt-daily
                #:cancel-task
                #:list-tasks
                #:scheduler-status
                #:get-schedulable-action
                #:list-schedulable-actions
                ;; task persistence
                #:persist-user-tasks
                #:list-unrestorable-tasks
                ;; weather / system
                #:weather-report
                #:system-report
                #:system-monitor-config
                #:start-system-monitoring
                #:stop-system-monitoring
                ;; battery
                #:battery-report
                #:start-battery-monitoring
                #:stop-battery-monitoring
                ;; time / ephemeris
                #:current-time-plist
                #:current-time-report
                #:ephemeris-report
                ;; rss monitors
                #:rss-report
                #:rss-check-report
                #:rss-monitor-start
                #:rss-monitor-stop
                #:rss-monitor-stop-many
                #:rss-monitor-mute
                #:rss-monitor-unmute
                #:rss-reset-all-backoff
                #:rss-monitor-configs
                #:opml-import-monitors
                #:opml-export-monitors
                ;; rss feeds
                #:rss-feed-configure
                #:rss-feed-publish
                #:rss-feed-items
                #:rss-feed-xml
                #:rss-feed-clear
                #:rss-feed-delete
                #:rss-feed-list
                ;; meters / usage
                #:list-meters
                #:meter-recent
                #:meter-report
                #:usage-report
                ;; log inspector
                #:read-log-tail
                #:search-log
                #:log-report
                ;; amp
                #:amp-status
                #:amp-code-task
                #:amp-test-task
                #:amp-report
                ;; raindrop
                #:format-raindrop
                #:format-raindrop-list
                #:raindrop-get-one
                #:raindrop-update
                #:raindrop-remove
                #:raindrop-list
                #:raindrop-create-collection
                #:raindrop-save-report
                #:raindrop-find-report
                #:raindrop-collections-report
                #:raindrop-tags-report
                ;; books
                #:books-search
                #:books-by-tag
                #:books-by-tags
                #:books-by-author
                #:books-by-collection
                #:books-by-series
                #:books-get
                #:books-list-tags
                #:books-list-collections
                #:books-list-series
                #:books-status
                ;; orgmode
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
                #:pushover-report
                ;; github
                #:github-create-issue
                #:github-report
                ;; hoobs
                #:hoobs-get-accessory
                #:hoobs-set-accessory
                #:hoobs-rooms
                #:hoobs-report
                ;; git
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
                ;; retry / timeout
                #:with-timeout)
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


