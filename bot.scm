(require-extension riaxpander)

(define-syntax while
  (syntax-rules ()
    ((while cond) (letrec ((loop (lambda () (if cond (loop) #f)))) (loop)))
    ((while cond body ...) (letrec ((loop (lambda () (if cond (begin body ... (loop)) #f)))) (loop)))))

(define (write-all exp #!optional out)
  (if (not (null? exp))
      (begin
        (write (car exp) (if out out (current-output-port)))
        (newline (if out out (current-output-port)))
        (write-all (cdr exp) out))
      '()))

(require-extension remote-repl-server)
(rrepl-server-start 5040)

(tcp-read-timeout #f)

(require-extension irc)
(require-extension sandbox)

(load "quote_search.scm")

(define (read-all #!optional in) (let ((R (read (if in in (current-input-port)))))
                                   (if (eof-object? R)
                                       '()
                                       (cons R (read-all in)))))

;(define current-file-output (open-output-file "debug.log"))

;(define (get-current-file-output-port) current-file-output)

;(set! current-output-port get-current-file-output-port)
;(set! current-error-port get-current-file-output-port)

;(set-current-output-port! (open-output-file "debug.log"))
;(set-current-error-port! current-output-port)

(define bot-name "endeebot")
(define bot-server "irc.efnet.ch")
;(set! bot-server "efnet.xs4all.nl")
(define channel-list '("#weightedsixes"))
(define channel-name "#weightedsixes") ; only support one channel atm
(define op-file "channel.ops")
(define op-pass-file "pass.ops")
(define feature-file "features.todo")
(define irc-log-file "weightedsixes.log")

(define op-list '())
(define op-pass-list '())
(define temp-op-list '())
(define quit-func-list '())

(define (add-quit-func func)
  (set! quit-func-list (cons func quit-func-list)))

(define (call-quit-funcs)
  (map
   (lambda (func) (func))
   quit-func-list))

(define irc-log (open-output-file irc-log-file #:append))

(define (write-irc-log msg) (write msg irc-log) (newline irc-log) (flush-output irc-log))

(add-quit-func (lambda () (close-output-port irc-log)))

(define (read-op-list) (set! op-list (with-input-from-file op-file
                                       read)))

(define (write-op-list) (with-output-to-file op-file
                          (lambda () (write op-list))))

(define (read-op-pass-list) (set! op-pass-list
                                  (with-input-from-file op-pass-file
                                    read)))
(define (write-op-pass-list) (with-output-to-file op-pass-file
                               (lambda () (write op-pass-list))))

(define (add-feature-todo desc)
  (let ((feature-list (call-with-input-file feature-file read-all)))
    (call-with-output-file feature-file
      (lambda (out) (write-all (append feature-list (cons desc '())) out)))))

(read-op-list)
(read-op-pass-list)

(define (join-channels lst)
  (if (not (null? lst))
      (begin
        (irc:join con (car lst))
        (join-channels (cdr lst)))
      '()))

(define (message-body message)
  (let ((parameters (cadr (irc:message-parameters message))))
    (if (irc:extended-data? parameters)
        (irc:extended-data-content parameters)
        parameters)))

(define (message-hostmask msg)
  (string-append (cadr (irc:message-prefix msg)) "@" (caddr (irc:message-prefix msg))))

(define (is-in-op-list? name host lst)
  (find
   (lambda (op)
     (and (equal? (car op) name)
          (string-match (glob->regexp (cdr op)) host)))
   lst))

(define (is-op? name host)
  (or (is-in-op-list? name host op-list)
      (is-in-op-list? name host temp-op-list)))

(define (message-dest msg)
  (if (equal? bot-name (irc:message-receiver msg))
      (irc:message-sender msg)
      channel-name))

(define con (irc:connection nick: bot-name server: bot-server))
(irc:connect con)

(join-channels channel-list)

;(irc:remove-message-handler! con 'QUIT)

(irc:add-message-handler! con (lambda (msg) (display "quit!") (irc:quit con "Bye") (call-quit-funcs) (exit))
                          sender: (lambda (sender) (equal? sender "Arelius"))
                          body: (string-append "PRIVMSG .* :" bot-name "[,: ]*QUIT") tag: 'QUIT)


;(irc:remove-message-handler! con 'op)

(irc:add-message-handler! con (lambda (msg)
                                (if (is-op? (irc:message-sender msg) (message-hostmask msg))
                                    (irc:command
                                     con
                                     (string-append "mode " channel-name " +o " (irc:message-sender msg)))))
                          command: "JOIN"
                          tag: 'op)

;(irc:remove-message-handler! con 'deop)

(irc:add-message-handler! con
                          (lambda (msg)
                            (set! temp-op-list
                                  (remove
                                   (lambda (op) (equal? (message-hostmask msg) (cdr op)))
                                   temp-op-list)))
                          command: "PART"
                          tag: 'deop)

(define (get-bot-command msg)
  (let ((out (string-match (string-append
                            "^" bot-name "[,:]?[ ]*(.*)")
                           (message-body msg))))
    (if out
        (handle-exceptions exn
                           #f
                           (call-with-input-string (cadr out) read-all))
        #f)))

(define (run-command msg)
  (handle-exceptions
   exn
   #f
   (match (get-bot-command msg)
          ; Tempory Op
          (('op 'me pass)
           (if (find (lambda (op-pass) (equal? op-pass (cons (irc:message-sender msg) (->string pass))))
                     op-pass-list)
               (begin
                 (set! temp-op-list
                       (cons
                        (cons (irc:message-sender msg) (message-hostmask msg))
                        temp-op-list))
                 (irc:command
                  con
                  (string-append "mode " channel-name " +o " (irc:message-sender msg))))
               '()))
          ; Eval
          (('eval exp)
           (handle-exceptions exn
                              (irc:say con
                                       "Error in eval!"
                                       (message-dest msg))
                              (irc:say con
                                       (->string
                                        (safe-eval
                                         exp
                                         fuel: 1000
                                         allocation-limit: 1048576))
                                       (message-dest msg))))
          )))

(define (run-admin-command msg)
  (handle-exceptions
   exn
   #f
   (match (get-bot-command msg)
          ;; Add Op
          (('add 'op user hostmask)
           (set! op-list
                 (cons
                  (cons (->string user) (->string hostmask))
                  op-list))
           (write-op-list)
           (irc:say con
                    (string-append
                     "Added user \"" (->string user) "\" to op list.")
                    (message-dest msg))
           #t)
                                        ; Remove Op
          (('remove 'op user)
           (set! op-list
                 (remove
                  (lambda (curr-op)
                    (if (equal? (car curr-op) (->string user))
                        (begin
                          (irc:say con
                                   (string-append
                                    "Removed user \"" (->string user) "\" with hostmask \"" (->string (cdr curr-op)) "\" from op list.")
                                   (message-dest msg))
                          #t)
                        #f)) op-list))
           (write-op-list))
                                        ; Remove Op (With hostmask)
          (('remove 'op user hostmask)
           (set! op-list
                 (remove
                  (lambda (curr-op)
                    (if (equal? curr-op (cons (->string user) (->string hostmask)))
                        (begin
                          (irc:say con
                                   (string-append
                                    "Removed user \"" (->string user) "\" with hostmask \"" (->string (cdr curr-op)) "\" from op list.")
                                   (message-dest msg))
                          #t)
                        #f)) op-list))
           (write-op-list))
                                        ; Temp Op Pass set
          (('set 'pass pass)
           (set! op-pass-list
                 (cons
                  (cons
                   (irc:message-sender msg)
                   (->string pass))
                  (remove
                   (lambda (op)
                     (equal? (car op) (irc:message-sender msg)))
                   op-pass-list)))
           (write-op-pass-list)
           (irc:say con
                    (string-append
                     "Added Pass for user \"" (irc:message-sender msg) "\".")
                    (message-dest msg)))
                                        ; Feature Requiest
          (('add 'bot 'todo rest ...)
           (add-feature-todo rest)
           (irc:say con
                    "Added todo to bot feature list."
                    (message-dest msg)))
          ; Eval
          (('op 'eval exp)
           (handle-exceptions exn
                              (irc:say con
                                       "Error in eval!"
                                       (message-dest msg))
                              (irc:say con
                                       (->string
                                        (eval
                                         exp))
                                       (message-dest msg)))))))


;(irc:remove-message-handler! con 'admin)

(irc:add-message-handler! con (lambda (msg)
                                (if (get-bot-command msg)
                                    (if (is-op? (irc:message-sender msg) (message-hostmask msg))
                                        (run-admin-command msg)
                                        #f)
                                    #f))
                          body: bot-name
                          command: "PRIVMSG"
                          tag: 'admin)

;(irc:remove-message-handler! con 'log)

(irc:add-message-handler! con (lambda (msg)
                                (if (not (equal? bot-name (irc:message-sender msg)))
                                    (write-irc-log `(,(current-seconds) ,(->string (irc:message-sender msg)) ,(->string (message-body msg))))
                                    '())
                                #f)
                          command: "PRIVMSG"
                          tag: 'log)

;(irc:remove-message-handler! con 'command)

(irc:add-message-handler! con (lambda (msg)
                                (if (get-bot-command msg)
                                    (run-command msg)
                                    (irc:say con
                                             (string-trim-both (match-quote-string (message-body msg)))
                                             (message-dest msg))))
                          body: bot-name
                          command: "PRIVMSG"
                          tag: 'command)

(define (run) (irc:run-message-loop con debug: #t pong: #t))

;(run)
(while #t (handle-exceptions exn '() (run))) ; To Catch rrepl caused exceptions