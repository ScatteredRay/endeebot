(require-extension riaxpander)

(define-syntax while
  (syntax-rules ()
    ((while cond) (letrec ((loop (lambda () (if cond (loop) #f)))) (loop)))
     ((while cond body ...) (letrec ((loop (lambda () (if cond (begin body ... (loop)) #f)))) (loop)))))

(require-extension remote-repl-server)
(rrepl-server-start 5040)

(tcp-read-timeout #f)

(require-extension irc)
(require-extension sandbox)

(define bot-name "endeebot")
(define bot-server "irc.efnet.ch")
;(set! bot-server "efnet.xs4all.nl")
(define channel-list '("#weightedsixes"))
(define channel-name "#weightedsixes") ; only support one channel atm

(define op-file "channel.ops")
(define op-list '())

(define (read-op-list) (set! op-list (with-input-from-file op-file
                                       read)))

(define (write-op-list) (with-output-to-file op-file
                                               (lambda () (write op-list))))

(read-op-list)

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

(define (is-op? name host)
  (find
   (lambda (op)
     (and (equal? (car op) name)
          (string-match (cdr op) host)))
   op-list))

(define (message-dest msg)
  (if (equal? bot-name (irc:message-receiver msg))
      (irc:message-sender msg)
      channel-name))

(define con (irc:connection nick: bot-name server: bot-server))
(irc:connect con)

(join-channels channel-list)

;(irc:remove-message-handler! con 'QUIT)

(irc:add-message-handler! con (lambda (msg) (display "quit!") (irc:quit con "Bye") (exit))
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

(define (get-bot-command msg)
  (let ((out (string-match (string-append
                            "^" bot-name "[,:]?[ ]*(.*)")
                           (message-body msg))))
    (if out
        (cadr out)
        #f)))

(define (run-admin-command msg)
  (let ((cmd (string-match
              "add op[ ]*([^ ]*)[ ]*([^ ]*)"
              (get-bot-command msg))))
    (if cmd
        (begin
          (set! op-list
                (cons
                 (cons (cadr cmd) (caddr cmd))
                 op-list))
          (write-op-list)
          (irc:say con
                   (string-append
                    "Added user \"" (cadr cmd) "\" to operator list.")
                   (message-dest msg)))
        '())))

;(irc:remove-message-handler! con 'admin)

(irc:add-message-handler! con (lambda (msg)
                                (if (is-op? (irc:message-sender msg) (message-hostmask msg))
                                    (if (get-bot-command msg)
                                        (if (run-admin-command msg)
                                            #t
                                            #f)
                                        #f))
                                #f)
                          body: bot-name
                          command: "PRIVMSG"
                          tag: 'admin)

;(irc:remove-message-handler! con 'eval)

(irc:add-message-handler! con (lambda (msg)
                                (let* ((search (string-search
                                               (string-append "PRIVMSG (.*) :" bot-name "[,: ]*eval(.*)")
                                               (irc:message-body msg)))
                                      (resp-dest (message-dest msg)))
                                  (handle-exceptions exn
                                                     (irc:say con
                                                              "Error in eval!"
                                                              resp-dest)
                                                     (irc:say con
                                                              (->string
                                                               (safe-eval
                                                                (read
                                                                 (open-input-string
                                                                  (caddr search)))
                                                                fuel: 1000
                                                                allocation-limit: 1048576))
                                                              resp-dest))))
                          sender: (lambda (sender) #t)
                          body: (string-append "PRIVMSG (.*) :" bot-name "[,: ]*eval(.*)")
                          tag: 'eval
                          command: "PRIVMSG")


(define (run) (irc:run-message-loop con debug: #t pong: #t))

(run)
;(while (#t) (handle-exceptions exn '() (run))) ; To Catch rrepl caused exceptions