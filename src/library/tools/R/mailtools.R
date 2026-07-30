mailx <-
function(subject = "", address, body = character(),
         cc, bcc, from = Sys.getenv("EMAIL"), replyto,
         verbose = FALSE, headers = character(), nail = TRUE)
{
    if(missing(address)) stop("must specify 'address'")
    if(!nzchar(subject)) stop("'subject' is missing")

    args <- c("-s", shQuote(subject))
    blat <- .Platform$OS.type == "windows" && Sys.which("blat") != ""

    if(blat) {
        if(!missing(address))
            args <- c(args, "-to", shQuote(paste(address, collapse = ",")))
        if(!missing(cc) && length(cc))
            args <- c(args, "-cc", shQuote(paste(cc, collapse = ",")))
        if(!missing(bcc) && length(bcc))
            args <- c(args, "-bcc", shQuote(paste(bcc, collapse = ",")))
        if(nzchar(from))
            args <- c(args, "-f", shQuote(from))
        if(!missing(replyto) && nzchar(replyto))
            args <- c(args, "-replyto", shQuote(replyto))
        for(h in headers)
            args <- c(args, "-x", shQuote(h))
    }
    else {
        ## Argh.
        ## We need to be able to send mail with custom headers added,
        ## for which there seems no simple and portable solution.
        ## Basic POSIX mailx provides no such functionality, see
        ## <https://pubs.opengroup.org/onlinepubs/9699919799/utilities/mailx.html>
        ## The above even says
        ##   Options to specify addresses as cc (carbon copy) or bcc
        ##   (blind carbon copy) were considered to be format details
        ##   and were omitted.
        ## It also says
        ##   If the User Portability Utilities option is supported, then
        ##   in both Send and Receive Modes, standard input lines
        ##   beginning with the escape character (usually <tilde>) shall
        ##   affect processing as described in Command Escapes in mailx
        ## which seems rather unclear, and apparently even when
        ## available does not seem to provide for custom headers.
        ## See also
        ##   <https://unix.stackexchange.com/questions/30599/setting-headers-using-the-bin-mail-command>
        ##   <https://unix.stackexchange.com/questions/90526/how-can-i-set-an-e-mails-headers-using-usr-bin-mail>
        ##   <https://stackoverflow.com/questions/31053337/how-to-add-x-header-to-unix-mailx-or-add-attachment-to-usr-sbin-sendmail>
        ##   <https://stackoverflow.com/questions/1296979/how-to-set-the-from-email-address-for-mailx-command>
        ## Apparently
        ##   sendmail s-nail mutt mail.mailutils
        ## all provide what is needed, but may not be available on the
        ## system (e.g., there no longer seems a Fedora package for GNU
        ## Mailutils).
        ## So for now try to use
        ##   s-nail <https://www.sdaoden.eu/code.html>
        ## if available which has '-C' for custom headers.
        ## Otherwise, on Debian/Ubuntu we can use mailx from the
        ## bsd-mailx package which is based on 
        ##   <http://cvsweb.openbsd.org/cgi-bin/cvsweb/src/usr.bin/mail/>
        ## and adds '-a' for custom headers.
        ## Both these have options
        ##   -c -b -r
        ## for cc, bcc and from.  Neither has an option for replyto: 
        ## s-nail has variable
        ##   reply-to
        ## and bsd-mailx has environment variable
        ##   REPLYTO
        ## Note that s-nail does not allow -C for overriding standard
        ## headers including Reply-To, so we use -S for setting the
        ## variable.
        env <- character()
        mailx <- Sys.which("s-nail")
        if(nzchar(mailx) && nail)
            headopt <- "-C"
        else {
            nail <- FALSE
            mailx <- "mailx" # Hope for the best ...
            headopt <- "-a"
        }
        ## Argh.  s-nail and mailx both have -c and -b, but for multiple
        ## addresses s-nail wants repeated args and mailx wants one as a
        ## comma-separate list.
        if(!missing(cc) && length(cc))
            args <- c(args,
                      if(nail) {
                          paste("-c", shQuote(cc))
                      } else {
                          c("-c", shQuote(paste(cc, collapse = ", ")))
                      })
        if(!missing(bcc) && length(bcc))
            args <- c(args,
                      if(nail) {
                          paste("-b", shQuote(bcc))
                      } else {
                          c("-b", shQuote(paste(bcc, collapse = ", ")))
                      })
        if(nzchar(from))
            args <- c(args, "-r", shQuote(from))
        if(!missing(replyto) && nzchar(replyto)) {
            if(nail)
                args <- c(args, "-S",
                          sprintf("reply-to=%s", shQuote(replyto)))
            else {
                headers <- c(paste("Reply-To:", replyto), headers)
                ## Alternatively, can use
                ##   env <- c(env,
                ##            sprintf("REPLYTO=%s", shQuote(replyto)))
                ## However, when not doing so, one needs to ensure that
                ## there is REPLYTO environment variable, as otherwise
                ## bsd-mailx will happily create emails with two
                ## Reply-To fields, which e.g. Gmail will reject with
                ##   This message is not RFC 5322 compliant.
                ##   There are multiple Reply-To 550-5.7.1 headers.
                Sys.unsetenv("REPLYTO")
            }
        }
            
        for(h in headers)
            args <- c(args, headopt, shQuote(h))
        
        address <- paste(shQuote(address), collapse = " ")
    }


    filename <- sprintf("R_post_%s", format(Sys.time(), "%Y%m%d_%H%M%S"))
    cat(body, file = filename, sep = "\n")

    ## <NOTE>
    ## To avoid reading the user's configuration files for general
    ## purpose use, the man page suggests using
    ##   MAILRC=/dev/null mailx -n
    ## and create a configuration file for the script.
    ## </NOTE>
    
    if(verbose)
        message(sprintf("Sending email to %s", address))

    if(blat)
        status <- shell(paste("blat", filename, paste(args, collapse=" ")))
    else
        status <- system2(mailx, c(args, address), env = env,
                          stdin = filename, stdout = FALSE, stderr = "")
    if(status == 0L) unlink(filename)
    else {
        message(sprintf("Sending email failed!\nThe unsent msg can be found in file %s.",
                        sQuote(filename)))
    }
    
    invisible()
}

mailx_from_head_and_body_list <-
function(x, from = Sys.getenv("EMAIL"),
         verbose = FALSE, headers = character())
{
    h <- x$head
    replyto <- h[["Reply-To"]] # done initially do avoid scoping issues
    headers <- c(h$headers, headers)
    mailx(h$Subject,
          h$To,
          body = x$body,
          from = from,
          cc = h$CC,
          bcc = h$Bcc,
          replyto = replyto,
          verbose = verbose,
          headers = headers)
}
