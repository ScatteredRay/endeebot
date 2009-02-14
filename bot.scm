(require-extension riaxpander)

(define-syntax while
  (syntax-rules ()
    ((while cond) (letrec ((loop (lambda () (if cond (loop) #f)))) (loop)))
     ((while cond body ...) (letrec ((loop (lambda () (if cond (begin body ... (loop)) #f)))) (loop)))))

(require-extension remote-repl-server)
(rrepl-server-start 5040)

(tcp-read-timeout #f)

(require-extension irc)

(define bot-name "endeebot")
(define bot-server "irc.efnet.ch")
;(set! bot-server "efnet.xs4all.nl")

(define con (irc:connection nick: bot-name server: bot-server))

(irc:connect con)

;; (define (dump-message msg)
;;   (display (irc:message-prefix msg))
;;   (newline)
;;   (display (irc:message-command msg))
;;   (newline)
;;   (display (irc:message-code msg))
;;   (newline)
;;   (display (irc:message-body msg))
;;   (newline))

;; (define curr-msg '())

;; (define (wait-dump)
;;   (set! curr-msg (irc:wait con))
;;   (dump-message curr-msg))

;(define (bot-eval msg))

(irc:join con "#weightedsixes")

(irc:say con "Hello" "Arelius")

;(irc:remove-message-handler! con 'QUIT)

(irc:add-message-handler! con (lambda (msg) (display "quit!") (irc:quit con "Bye") (exit))
                          sender: (lambda (sender) (or (equal? sender "Arelius") (equal? sender "pf_")))
                          body: (string-append "PRIVMSG .* :" bot-name "[,: ]*QUIT") tag: 'QUIT)

;(irc:remove-message-handler! con 'eval)

(irc:add-message-handler! con (lambda (msg)
                                (handle-exceptions exn
                                                   '()
                                                   (let ((search (string-search
                                                                  (string-append "PRIVMSG (.*) :" bot-name "[,: ]*eval(.*)")
                                                                  (irc:message-body msg))))
                                                     (irc:say con
                                                              (->string
                                                               (eval
                                                                (read
                                                                 (open-input-string
                                                                  (caddr search)))))
                                                              (if (equal? bot-name (cadr search))
                                                                  (irc:message-sender msg)
                                                                  (cadr search))))))
                          sender: (lambda (sender) (equal? sender "Arelius"))
                          body: (string-append "PRIVMSG (.*) :" bot-name "[,: ]*eval(.*)") tag: 'eval)


(define (run) (irc:run-message-loop con debug: #t pong: #t))

(run)
;(while (#t) (handle-exceptions exn '() (run))) ; To Catch rrepl cause exceptions

;(irc:quit con "Buh bye")
;(irc:disconnect con)