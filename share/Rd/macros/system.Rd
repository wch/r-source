% System Rd macros

% These macros are automatically loaded whenever R processes an Rd file.

% Packages may define their own macros, which are stored in man/macros/*.Rd in
% the source, help/macros/*.Rd after installation.  Those will be processed after
% this file but before every Rd file in a package.

% Packages may request inclusion of macros from other packages using the 
% LoadRdMacros line in the DESCRIPTION file, e.g.
%  LoadRdMacros:  pkgA 
% These are loaded after the system macros and before the current package macros.

% Individual Rd files may define their own macros.



% To refer to a package on CRAN
\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

% To refer to a bug report by number
\newcommand{\PR}{\Sexpr[results=rd]{tools:::Rd_expr_PR(#1)}}

% To avoid a double space after a period in LaTeX output
\newcommand{\sspace}{\ifelse{latex}{\out{~}}{ }}
