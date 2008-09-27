bug.report <- function(subject = "", ccaddress = Sys.getenv("USER"),
                       method = getOption("mailer"),
                       address = "r-bugs@r-project.org",
                       file = "R.bug.report")
{
    create.post(instructions = "\\n<<insert bug report here>>\\n\\n\\n\\n",
                description = "bug report",
                subject = subject,
                ccaddress = ccaddress,
                method = method,
                address = address,
                file = file)
}
