;;;; package.lisp
;;;;
;;;; Package definitions for Crichton

(defpackage #:crichton/config
  (:use #:cl)
  (:export #:*agent-home*
           #:*config*
           #:agent-path
           #:ensure-directories
           #:load-config
           #:reload-config
           #:config-get
           #:config-section-get))

(defpackage #:crichton/logging
  (:use #:cl)
  (:export #:setup-logging
           #:suppress-console-logging
           #:with-redaction))

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
           #:daemon-socket-path))

(defpackage #:crichton/cli
  (:use #:cl)
  (:export #:main
           #:remote-eval
           #:remote-eval-quiet))

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
           #:run-wasm-module
           #:run-wasm-with-host-fns
           #:run-wasm-bytes-with-host-fns
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
           #:manifest-error
           #:manifest-error-path
           #:manifest-error-problems
           #:generate-signing-keypair
           #:sign-skill-bytes
           #:verify-skill-signature
           #:load-trusted-keys
           #:verify-skill-bundle
           #:weather-report
           #:weather-conditions
           #:system-loadavg
           #:system-memory
           #:system-thermal-zones
           #:system-disk-usage
           #:system-snapshot
           #:system-report
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
           #:rss-list-monitors
           #:clear-seen
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
           #:kv-quota-exceeded))

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
           #:llm-feature-not-supported
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
           #:ask
           #:chat-session
           #:register-all-tools
           #:all-tool-defs
           #:dispatch-tool
           #:register-tool))

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

(defpackage #:crichton
  (:use #:cl)
  (:export #:main))
