% This class file is provided for authors submitting to the
% bioRxiv
%
% Version: 0.1, 2018/03/07
%

%
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{rilabRxiv}[03/07/2018, v0.1]
\AtEndOfClass{\RequirePackage{microtype}}

% Options for element switching
\RequirePackage{xifthen}
\newboolean{displaycopyright}
\newboolean{displaylineno}
\setboolean{displaylineno}{false}

\DeclareOption{lineno}{\setboolean{displaylineno}{true}}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{extarticle}}
\ProcessOptions*
\LoadClass{extarticle}

% April 16, 2016:
% Line numbers
\RequirePackage[running,switch]{lineno}
\ifthenelse{\boolean{displaylineno}}{\AtBeginDocument{\linenumbers}}{}

% Fonts and language
\RequirePackage[utf8]{inputenc}
\RequirePackage[english]{babel}
\RequirePackage{amsmath,amsfonts,amssymb}
\RequirePackage{mathpazo}
\RequirePackage[scaled]{helvet}
\RequirePackage[T1]{fontenc}
\RequirePackage{url}
\RequirePackage[colorlinks=true, allcolors=blue]{hyperref}
\RequirePackage{lettrine}

\newcommand{\watermarkfont}{\color{black50}\normalfont\fontsize{10}{12}\selectfont}
\newcommand{\headerfont}{\color{color2}\normalfont\sffamily\bfseries\fontsize{10}{12}\selectfont}
\newcommand{\titlefont}{\normalfont\sffamily\bfseries\fontsize{21}{23}\selectfont}
\newcommand{\datesfont}{\normalfont\sffamily\itshape\fontsize{8}{10}\selectfont}
\renewcommand\UrlFont{\color{color1}\sffamily}
\newcommand{\absfont}{\normalfont\sffamily\linespread{1.2}\fontsize{10}{12}\selectfont}
\newcommand{\keywordsfont}{\normalfont\sffamily\linespread{1.2}\fontsize{8}{10}\selectfont}
\newcommand{\copyrightfont}{\normalfont\sffamily\fontsize{7}{9}\selectfont}

\RequirePackage{datetime}

\newdateformat{monthyeardate}{%
  \monthname[\THEMONTH] \THEYEAR}

\RequirePackage{enumitem} 
\setlist[itemize]{noitemsep}

% Choose article type
\newcommand*{\articletype}[1]{%
%  \RequirePackage{styles/#1} Possibility to add style file
  }

% Author and affiliation
\RequirePackage{authblk}
\setlength{\affilsep}{0.1em}
\renewcommand\Authfont{\color{color0}\normalfont\sffamily\bfseries\fontsize{9}{13}\selectfont}
\renewcommand\Affilfont{\color{color0}\normalfont\sffamily\fontsize{8}{10}\selectfont}
\renewcommand\AB@affilsepx{, \protect\Affilfont}
\renewcommand\Authands{ and }


% Copyright statement
\RequirePackage{textcomp} % For copyright symbol styling
\newcommand{\copyrightstatement}{Copyright \, \textcopyright\, \the\year{} by the authors}

% Graphics, tables and other formatting
\RequirePackage{graphicx,xcolor}
\RequirePackage{colortbl}
\RequirePackage{booktabs}
\RequirePackage{tikz}
\RequirePackage{algorithm}
\RequirePackage[noend]{algpseudocode}
\RequirePackage{changepage}
\RequirePackage[left=48pt,%
                right=42pt,%
                top=48pt,%
                bottom=60pt,%
                headheight=25pt,%
                headsep=10pt,%
                letterpaper,twoside]{geometry}%
\RequirePackage[labelfont={bf,sf},%
                labelsep=space,%
                figurename=Figure,%
                singlelinecheck=off,%
                justification=RaggedRight]{caption}
\setlength{\columnsep}{24pt} % Distance between the two columns of text
\setlength{\parindent}{12pt} % Paragraph indent

% Set document color scheme
\definecolor{black50}{gray}{0.5} % 50% black for hrules
\definecolor{color0}{RGB}{0,0,0} % Base color
\definecolor{color1}{RGB}{59,90,198} % unused
\definecolor{color2}{RGB}{189,38,53} % unused
% color2 defined by the specific article type style file

% Bibliography
\usepackage{natbib} \bibpunct{(}{)}{;}{author-year}{}{,}
% \bibliographystyle{genetics}
\bibliographystyle{pnas-new}

\addto{\captionsenglish}{\renewcommand{\refname}{References}}
\setlength{\bibsep}{0.0pt}

% Table style
\arrayrulecolor{color0} % Set the color of the horizontal rules
\renewcommand{\arraystretch}{1.5} % Increased line spacing
\captionsetup[table]{position=below} % Reduce spacing below caption

% Algorithm style
\newcommand\fs@coloruled{\def\@fs@cfont{\bfseries}\let\@fs@capt\floatc@coloruled
  \def\@fs@pre{\kern2pt}%
  \def\@fs@post{\kern2pt{\color{color2}\hrule}\relax}%
  \def\@fs@mid{\kern2pt{\color{color2}\hrule}\kern2pt}%
  \let\@fs@iftopcapt\iftrue}

\floatstyle{coloruled}
\restylefloat{algorithm}

% Headers and footers
\RequirePackage{fancyhdr}  % custom headers/footers
\RequirePackage{lastpage}  % Number of pages in the document
\pagestyle{fancy}          % Enables the custom headers/footers

\fancypagestyle{firststyle}{
   \fancyhead[R]{{\watermarkfont bio\textcolor{color2}{R}$\chi$iv}}
   %\fancyhead[L]{\includegraphics[width=0.8cm]{cornkey.png}}
   \fancyfoot[R]{\footnotesize bio\textcolor{color2}{R}$\chi$iv \hspace{1em} \monthyeardate\today \hspace{2em} \thepage\ }
}

% Headers
\fancyhead[LO,RE,LE,RO]{} % Blank headers for non-title pages.
% Footers
\lfoot{}
\cfoot{}
\rfoot{}
% \makeatletter
\fancyfoot[LE]{\thepage\ \hspace{2em} \@runningauthor \hspace{0.2em}}
\fancyfoot[RO]{\@runningtitle \hspace{2em} \thepage\ }
% \makeatother
\fancyfoot[LO,RE]{}
\renewcommand{\headrulewidth}{0pt}% % No header rule
\renewcommand{\footrulewidth}{0pt}% % No footer rule

%\fancyfoot[LO,RE]{}
\renewcommand{\headrulewidth}{0pt}% % No header rule
\renewcommand{\footrulewidth}{0pt}% % No footer rule

%% 13 July 2016: make sure sections are unnumbered
\setcounter{secnumdepth}{0}
\setcounter{tocdepth}{0}

% Section/subsection/paragraph set-up
\RequirePackage[explicit]{titlesec}
\renewcommand{\thesubsection}{\Alph{subsection}}

\titleformat{\section}
  {\large\sffamily\bfseries}
  {\thesection.}
  {0.5em}
  {#1}
  []
\titleformat{name=\section,numberless}
  {\large\sffamily\bfseries}
  {}
  {0em}
  {#1}
  []
\titleformat{\subsection}
  {\sffamily\bfseries\itshape}
  {\thesubsection.}
  {0.5em}
  {#1}
  []
\titleformat{\subsubsection}[runin]
  {\sffamily\bfseries\itshape}
  {\thesubsubsection.}
  {0.5em}
  {#1}
  []
\titleformat{\paragraph}[runin]
  {\sffamily\small\bfseries}
  {}
  {0em}
  {#1}
\titlespacing*{\section}{0pc}{3ex \@plus4pt \@minus3pt}{5pt}
\titlespacing*{\subsection}{0pc}{2.5ex \@plus3pt \@minus2pt}{2pt}
\titlespacing*{\subsubsection}{0pc}{2ex \@plus2.5pt \@minus1.5pt}{2pt}
\titlespacing*{\paragraph}{0pc}{1.5ex \@plus2pt \@minus1pt}{12pt}

% Article meta data additional fields
\newcommand{\runningauthor}[1]{\def\@runningauthor{#1}}
%% Backward compatibility
\newcommand{\correspondingauthor}[1]{\runningauthor{#1}}
\newcommand{\keywords}[1]{\def\@keywords{#1}}
\newcommand{\runningtitle}[1]{\def\@runningtitle{#1}}

\newcommand{\keywordname}{KEYWORDS} % Defines the keywords heading name

% Colored margin box
\newcommand{\marginmark}{}

\def\xabstract{abstract}
\long\def\abstract#1\end#2{\def\two{#2}\ifx\two\xabstract
\long\gdef\theabstract{\ignorespaces#1}
\def\go{\end{abstract}}\else
\typeout{^^J^^J PLEASE DO NOT USE ANY \string\begin\space \string\end^^J
COMMANDS WITHIN ABSTRACT^^J^^J}#1\end{#2}
\gdef\theabstract{\vskip12pt BADLY FORMED ABSTRACT: PLEASE DO
NOT USE {\tt\string\begin...\string\end} COMMANDS WITHIN
THE ABSTRACT\vskip12pt}\let\go\relax\fi
\go}

% Set environment for abstract content and styling
\newcommand{\abscontent}{
\noindent
{%
\parbox{\dimexpr\linewidth}{%
	\ifthenelse{\boolean{displaylineno}}{\internallinenumbers}{}%
    {\absfont\bfseries ABSTRACT} \absfont \theabstract
}%
\vskip10pt%
\noindent
\parbox{\dimexpr\linewidth-2\fboxsep\relax}{%
\ifthenelse{\boolean{displaylineno}}{\internallinenumbers}{}%
{\keywordsfont\bfseries\keywordname}\hspace*{0.1em} \keywordsfont \@keywords%
}%
}%
}%

% Custom title page
\renewcommand{\@maketitle}{\bgroup\setlength{\parindent}{0pt}
\begin{flushright}
{%
\vskip30pt%
{\raggedleft \titlefont \@title\par}%
\vskip11pt
{\raggedleft \@author\par}
\vskip15pt%
}%
\end{flushright}\egroup
{%
{\color{color2}\rule{\linewidth}{0.8pt}}
\vskip12pt%
\abscontent
}%
\vskip15pt%
}%

% Other packages
\RequirePackage{enumitem} % For reducing bullet list item separation

\RequirePackage{tabularx}

\RequirePackage[flushmargin,ragged]{footmisc}

\newlength{\myFootnoteWidth}%
\newlength{\myFootnoteLabel}%
\setlength{\myFootnoteLabel}{0.5em}
\renewcommand{\@makefntext}[1]{%
  \setlength{\myFootnoteWidth}{\columnwidth}%
  \addtolength{\myFootnoteWidth}{-\myFootnoteLabel}%
  \noindent\makebox[\myFootnoteLabel][r]{\@makefnmark\ }%
  \parbox[t]{\myFootnoteWidth}{#1}
  }

% Commands to set the footnote for the front page.
\renewcommand{\footnoterule}{%
  \kern -3pt
  {\color{black50} \hrule width \columnwidth height 0.5pt}
  \kern 2.5pt
}

\newcommand{\@authfootnote}[2]{%
  \begingroup
  \setlength{\myFootnoteLabel}{0em}
  \renewcommand\thefootnote{}\footnote{\footnotelayout\copyrightfont\textsuperscript{#1}#2}%
   \endgroup%
}

\newcommand\blfootnote[1]{%
  \@authfootnote{}{#1}%
%   \begingroup
%   \setlength{\myFootnoteLabel}{0em}
%   \renewcommand\thefootnote{}\footnote{#1}%
%   \addtocounter{footnote}{-1}%
%   \addtocounter{Hfootnote}{-1}%
%   \endgroup%
}

\newcommand\correspondingauthoraffiliation[2]{%
  \@authfootnote{#1}{#2}%
%   \begingroup
%   \setlength{\myFootnoteLabel}{0em}
%   \renewcommand\thefootnote{}\footnote{\footnotelayout\copyrightfont\textsuperscript{1}#1}%
%   \endgroup
  %% v1.8: turn on line numbers after this
  \ifthenelse{\boolean{displaylineno}}{\par\linenumbers}{}
}

\newcommand{\equalcontrib}[1]{\@authfootnote{#1}{These authors contributed equally to this work.}}

\newcommand{\firstpagefootnote}{\blfootnote{\copyrightfont \ifthenelse{\boolean{displaycopyright}}{\copyrightstatement}{}\\%
Manuscript compiled: \today%
}}

\newenvironment{tableminipage}[1]{\begin{minipage}{#1}\renewcommand\footnoterule{ \kern -1ex}%
\setlength{\myFootnoteLabel}{0.5em}%
}{\end{minipage}}

% Package to enable absolute positioning of figures
\RequirePackage[absolute,overlay]{textpos}
\setlength{\TPHorizModule}{1mm}
\setlength{\TPVertModule}{1mm}
\textblockorigin{0mm}{0mm} % start everything near the top-left corner



