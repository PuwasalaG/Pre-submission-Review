%% monashthesis Class for LaTeX2e.
%% Version 2.1
%% By Rob J. Hyndman. 21 August 2014
%%
%% This class file is for postgraduate research students
%% in the Department of Econometrics & Business Statistics
%% at Monash University, Australia.

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{monashthesis}[2014/08/21 Monash Thesis Class]

\newif\ifsinglesp@
\newif\ifdoublesp@
\newif\ifsingleside@
\doublesp@true
\singleside@false
\DeclareOption{singleside}{\singleside@true}
\DeclareOption{doubleside}{\singleside@false}
\DeclareOption{singlespace}{\singlesp@true}
\DeclareOption{doublespace}{\doublesp@true}

%\DeclareOption{PhD}{\def\degreetitle{Doctor of Philosophy}}
\DeclareOption{PhD}{\def\degreetitle{Doctor of Philosophy}}
%\DeclareOption{Subject}{\def\subjecttitle{in Computer Science}}

\DeclareOption{MComm}{\def\degreetitle{Master of Commerce}}
\DeclareOption{MPhil}{\def\degreetitle{Master of Philosophy}}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{report}}

\ExecuteOptions{PhD}

\ProcessOptions

\ifsingleside@%
  \LoadClass[a4paper,oneside,openany,draft,11pt]{report}
\else%
  \LoadClass[a4paper,11pt,openright,twoside]{report}
\fi%

%% Page dimensions
\ifsingleside@%
  \usepackage[a4paper,left=2.5cm,right=2cm,top=3cm,bottom=2.5cm]{geometry}
\else%
  \usepackage[a4paper,left=3.5cm,right=2.5cm,top=3cm,bottom=2.5cm,twoside]{geometry}
\fi%

%% CAPTIONS
\RequirePackage{caption}
\DeclareCaptionStyle{italic}[justification=centering]
 {labelfont={bf},textfont={it},labelsep=colon}
\captionsetup[figure]{style=italic,format=hang,singlelinecheck=true}
\captionsetup[table]{style=italic,format=hang,singlelinecheck=true}

%% FONT
\RequirePackage{kpfonts}
%\RequirePackage{bera}
%\RequirePackage[charter,expert,sfscaled]{mathdesign}

%% HEADERS AND FOOTERS
\RequirePackage{fancyhdr}
\pagestyle{fancy}
\rfoot{\Large\sf\raisebox{-0.1cm}{\textbf{\thepage}}}
\lhead{\textsf{\expandafter{\@title}}}
\rhead{}
\cfoot{}
\setlength{\headheight}{15pt}
\renewcommand{\headrulewidth}{0.4pt}
\renewcommand{\footrulewidth}{0.4pt}
\fancypagestyle{plain}{%
\fancyhf{} % clear all header and footer fields
\fancyfoot[C]{\sf\thepage} % except the center
\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{0pt}}

%% MATHS
\newcommand\bmmax{2}
\RequirePackage{bm,amsmath}
\allowdisplaybreaks

%% GRAPHICS
\RequirePackage{graphicx}
\setcounter{topnumber}{2}
\setcounter{bottomnumber}{2}
\setcounter{totalnumber}{4}
\renewcommand{\topfraction}{0.85}
\renewcommand{\bottomfraction}{0.85}
\renewcommand{\textfraction}{0.15}
\renewcommand{\floatpagefraction}{0.7}

%% SECTION TITLES
\RequirePackage[sf,bf,compact]{titlesec}
%\titleformat*{\subsection}{\large,sf,bfshape}
\titlespacing{\section}{0pt}{3ex}{1ex}
\titlespacing{\subsection}{0pt}{2ex}{1ex}
\titlespacing{\subsubsection}{0pt}{1ex}{0ex}
\titleformat{\subsubsection}{\bf}{}{0ex}{}

%% SPACING
\RequirePackage{setspace}
\spacing{1}

%% LINE AND PAGE BREAKING
\sloppy
\clubpenalty = 10000
\widowpenalty = 10000
\brokenpenalty = 10000
\RequirePackage{microtype}

%% BIBLIOGRAPHY
%\RequirePackage{natbib}
\usepackage[natbib=true,backend=bibtex,style=authoryear-comp,dashed=false,maxbibnames=99,firstinits=true,maxcitenames=3,url=true,doi=false,isbn=false]{biblatex}

\DeclareFieldFormat{url}{\url{#1}}
\DeclareFieldFormat[article]{pages}{#1}
\DeclareFieldFormat[inproceedings]{pages}{\lowercase{pp.}#1}
\DeclareFieldFormat[incollection]{pages}{\lowercase{pp.}#1}
\DeclareFieldFormat[article]{volume}{\mkbibbold{#1}}
\DeclareFieldFormat[article]{number}{\mkbibparens{#1}}
\DeclareFieldFormat[article]{title}{\MakeCapital{#1}}
\DeclareFieldFormat[article]{url}{}
\DeclareFieldFormat[book]{url}{}
\DeclareFieldFormat[inbook]{url}{}
\DeclareFieldFormat[incollection]{url}{}
\DeclareFieldFormat[inproceedings]{url}{}
\DeclareFieldFormat[inproceedings]{title}{#1}
\DeclareFieldFormat{shorthandwidth}{#1}
% No dot before number of articles
\usepackage{xpatch}
\xpatchbibmacro{volume+number+eid}{\setunit*{\adddot}}{}{}{}
% Remove In: for an article.
\renewbibmacro{in:}{%
  \ifentrytype{article}{}{%
  \printtext{\bibstring{in}\intitlepunct}}}
% Get rid of months in dates (not sure that this works)
\AtEveryBibitem{\clearfield{month}}
\AtEveryCitekey{\clearfield{month}}

%% PARAGRAPH BREAKS
\setlength{\parskip}{1.4ex}
\setlength{\parindent}{0em}

%% HYPERLINKS
\RequirePackage{xcolor} % Needed for links
\definecolor{darkblue}{rgb}{0,0,.6}
\RequirePackage{url}
\RequirePackage[citecolor=0 0 0,colorlinks=false]{hyperref}
\hypersetup{
     breaklinks=true,
     bookmarksopen=true,
     bookmarksnumbered=true,
     linkcolor=darkblue,
     urlcolor=blue,
     citecolor=darkblue,
     colorlinks=true}

%% Set up line spacing
\AtBeginDocument{%
\ifsinglesp@%
  \singlespacing%
\else%
  \doublespacing%
\fi%
}


%% \author, \title are defined in report; here are the rest of the
%% front matter defining macros
\def\submitdate#1{\gdef\@submitdate{#1}}
\def\degrees#1{\gdef\@degrees{#1}}
\def\@title{}\def\@author{}
\def\@degrees{}
\def\@submitdate{\ifcase\the\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December\fi
  \space \number\the\year}

\def\titlepage{{\sf
		\thispagestyle{empty}%
		\null\vskip1in%
		\begin{center}
				\Huge\textbf{\expandafter{\@title}}
		\end{center}
		\vfill
		%\begin{center}\Large
		%		{A thesis submitted
		%		for the degree of\\[0.3cm] \degreetitle}
		%\end{center}
		
%		\vfill
%		\begin{center}
%				by\\[0.6cm]
%				{\LARGE\@author}\\[0.2cm]
%                {\@degrees}
%
%				\vfill\vfill
%
%				D\'{e}partement d'Informatique\\
%				Universit\'{e} Libre de Bruxelles\\Belgium
%		\end{center}
%				\begin{center}\large
%				\@submitdate
%		\end{center}

		\begin{center}\Large
			{Doctor of Philosophy}
		\end{center}
		\begin{center}\large
			{Pre-Submission Seminar}
		\end{center}
     		\vfill
     		\begin{center}\large
     			\textbf{Candidate:}\\
     		   		{Puwasala Gamakumara}
     		\end{center}     
		\vfill
	 	\begin{center}\large
	 		\textbf{Supervisors:}\\
	 		{Professor Rob J Hyndman}\\
	 		{Professor George Athanasopoulos}\\
			{Assoc Professor Anastasios Panagiotelis}
		\end{center}
		\vfill
  		\begin{center}\large
   			{Department of Econometrics and Business Statistics}\\
   			{Monash University}\\
   			{May, 2019}
   		\end{center}	
	 	\newpage\thispagestyle{empty}}}

\def\prefacesection#1{%
		\chapter*{#1}
		\addcontentsline{toc}{chapter}{#1}
		\markboth{#1}{#1}}

\def\thebibliography#1{\chapter*{\bibname\@mkboth
  {{\bibname}}{{\bibname}}}%
  \addcontentsline{toc}{chapter}{\bibname}%
  \list
  {\@biblabel{\arabic{enumiv}}}{\settowidth\labelwidth{\@biblabel{#1}}%
	\leftmargin\labelwidth
	\advance\leftmargin\labelsep
	\usecounter{enumiv}%
	\let\p@enumiv\@empty
	\def\theenumiv{\arabic{enumiv}}}%
	\def\newblock{\hskip .11em plus.33em minus.07em}%
	\sloppy\clubpenalty4000\widowpenalty4000
	\sfcode`\.=\@m}

\def\endthebibliography{%
  \def\@noitemerr{\@warning{Empty `thebibliography' environment}}%
  \endlist}


\numberwithin{section}{chapter}
\numberwithin{equation}{section}

\setcounter{tocdepth}{1}

\def\cleardoublepage{\clearpage\if@twoside \ifodd\c@page\else
\hbox{}
\vspace*{\fill}
\thispagestyle{empty}
\newpage
\if@twocolumn\hbox{}\newpage\fi\fi\fi}

\def\tighttoc
    {
        \dimen100 = \parskip
        \setlength{\parskip}{0pt}
        \tableofcontents
        \setlength{\parskip}{\dimen100}
    }

%Page numbering in Appendix
%\renewcommand{\thetable}{\Alph{chapter}\arabic{table}}
