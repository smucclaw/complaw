\NeedsTeXFormat{LaTeX2e}[1997/12/01]
\ProvidesClass{tlp}[2021/03/29 v1.0 TLP
  ^^Jdocument class]

\usepackage[figuresright]{rotating}
\usepackage{graphicx}
\usepackage{color}
\usepackage{amsmath}
\usepackage{ifpdf}

\newif\ifprodtf
\newif\ifonline\global\onlinefalse
\newif\ifnokeyword\global\nokeywordfalse
\newif\iferratum\global\erratumfalse
%\newif\ifprint\global\printfalse
\newif\ifmathtif\global\mathtiffalse%
\newif\ifprinter
\newif\ifpuretex\global\puretexfalse%
\newif\iffinals\global\finalsfalse
\newif\ifbakoma\global\bakomafalse
\newif\ifnoquery\global\noqueryfalse
\newif\if@issue\global\@issuefalse
\newif\ifOA\global\OAfalse

\newdimen\trimrule\trimrule0pt

\DeclareOption{oneside}{\relax}
\DeclareOption{nokeyword}{\nokeywordtrue}
\DeclareOption{erratum}{\global\erratumtrue}
\DeclareOption{twoside}{\@twosidetrue \@mparswitchtrue}
\DeclareOption{draft}{\setlength\overfullrule{5\p@}}
\DeclareOption{finals}{\global\finalstrue\setlength\overfullrule{\z@}}
\DeclareOption{issue}{\global\@issuetrue}
\DeclareOption{onecolumn}{\@twocolumnfalse}
\DeclareOption{twocolumn}{\relax}
\DeclareOption{titlepage}{\relax}
\DeclareOption{notitlepage}{\relax}
\DeclareOption{leqno}{\relax}
\DeclareOption{bakoma}{\global\bakomatrue\global\onlinetrue}
\DeclareOption{fleqn}{\relax}
\DeclareOption{noquery}{\global\noquerytrue}
\DeclareOption{prodtf}{\prodtftrue}
\DeclareOption{online}{\global\onlinetrue}
\DeclareOption{printer}{\setlength\overfullrule{0pt}\trimrule0pt\printertrue}
\DeclareOption{OA}{\global\OAtrue}


\ExecuteOptions{twoside,final,onecolumn}
\ProcessOptions\relax

\setlength\lineskip{1\p@}
\setlength\normallineskip{1\p@}
\renewcommand\baselinestretch{}

\renewcommand\normalsize{%
  \@setfontsize\normalsize\@xpt{13}%
  \abovedisplayskip 6.5\p@ \@plus 1\p@ \@minus 1\p@
  \belowdisplayskip \abovedisplayskip
  \abovedisplayshortskip 3\p@ \@plus 1\p@
  \belowdisplayshortskip \abovedisplayshortskip
  \let\@listi\@listI}

\normalsize

\newcommand\small{%
  \@setfontsize\small\@ixpt{11}%
  \abovedisplayskip 6\p@ \@plus 1\p@ \@minus 1\p@
  \belowdisplayskip \abovedisplayskip
  \abovedisplayshortskip 3\p@ \@plus 1\p@
  \belowdisplayshortskip \abovedisplayshortskip
  \def\@listi{\leftmargin\leftmargini
    \topsep 6\p@ \@plus 1\p@ \@minus 1\p@
    \parsep \z@ \itemsep \parsep}%
}

\newcommand\footnotesize{%
  \@setfontsize\footnotesize\@viiipt\@ixpt
  \abovedisplayskip 5\p@ \@plus 1\p@ \@minus 1\p@
  \belowdisplayskip \abovedisplayskip
  \abovedisplayshortskip \z@ \@plus 1\p@
  \belowdisplayshortskip \abovedisplayshortskip
  \def\@listi{\leftmargin\leftmargini
    \topsep 4.5\p@ \@plus 1\p@ \@minus 1\p@
    \parsep \z@ \itemsep \parsep}%
}

\newcommand\scriptsize{\@setfontsize\scriptsize\@viipt\@viiipt}
\newcommand\tiny{\@setfontsize\tiny\@vpt\@vipt}
\newcommand\large{\@setfontsize\large\@xiipt{14}}
\newcommand\Large{\@setfontsize\Large\@xivpt{18}}
%\ifprodtf
%  \newcommand\LARGE{\@setfontsize\LARGE{18}{21}}%
%\else
  \newcommand\LARGE{\@setfontsize\LARGE\@xviipt{21}}%
%\fi
\newcommand\huge{\@setfontsize\huge\@xxpt{25}}
\newcommand\Huge{\@setfontsize\Huge\@xxvpt{30}}

\newcommand\affilsize{\@setfontsize\affilsize\@viiipt\@xpt}
\newcommand\seven{\@setfontsize\seven\@viipt\@ixpt}
\let\authorsize\normalsize

\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
\DeclareRobustCommand*\cal{\@fontswitch\relax\mathcal}
\DeclareRobustCommand*\mit{\@fontswitch\relax\mathnormal}

%\ifprodtf \else
  \setlength\oddsidemargin{-12.4pt}%12.45pt
  \setlength\evensidemargin{-21.5pt}%20.75pt
  \setlength\topmargin{-44.45pt}
%\fi

\setlength{\paperwidth}{174truemm}
\setlength{\paperheight}{700.1bp}

\setlength\marginparwidth{2.0cm}
\setlength\marginparsep{10\p@}

\setlength\headheight{10\p@}
\setlength\headsep{12\p@}
\setlength\topskip{12\p@}
\setlength\footskip{26\p@}

\setlength\textheight{44\baselineskip}
\addtolength\textheight{\topskip}
%\setlength\textwidth{30pc}
%\addtolength{\textwidth}{22.25pt}
\setlength\textwidth{384pt}
\addtolength{\textheight}{15pt}
\setlength\columnsep{10\p@}
\setlength\columnseprule{\z@}


\setlength\footnotesep{\z@}
\setlength{\skip\footins}{19.5\p@ \@plus 12\p@ \@minus 1\p@}

\setlength\floatsep{13\p@ \@plus 6.5\p@ \@minus 1\p@}
\setlength\textfloatsep{11\p@ \@plus 3\p@ \@minus 2\p@}
\setlength\intextsep{12\p@ \@plus 3\p@ \@minus 2\p@}
\setlength\dblfloatsep{13\p@ \@plus 6.5\p@ \@minus 2\p@}
\setlength\dbltextfloatsep{11\p@ \@plus 3\p@ \@minus 2\p@}
\setlength\@fptop{\z@ \@plus 0fil}
\setlength\@fpsep{13\p@ \@plus 0fil}
\setlength\@fpbot{\z@ \@plus 3fil}
\setlength\@dblfptop{\z@ \@plus 0fil}
\setlength\@dblfpsep{13\p@ \@plus 0fil}
\setlength\@dblfpbot{\z@ \@plus 3fil}
\setlength\marginparpush{5\p@}

%\setlength\parskip{\z@ \@plus .3\p@}
\setlength\parskip{\z@}
\setlength\parindent{1em}
\setlength\partopsep{\z@ \@plus 1\p@}
\@lowpenalty   51
\@medpenalty  151
\@highpenalty 301
\@beginparpenalty -\@lowpenalty
\@endparpenalty -\@lowpenalty
\@itempenalty -\@lowpenalty
\clubpenalty\z@
\widowpenalty\@M

\ifpdf
\pdfpagewidth=\the\paperwidth
\pdfpageheight=\the\paperheight
\else
\AtBeginDvi{\special{papersize=\the\paperwidth,\the\paperheight}}
\fi


\newcommand\partname{Part}
\newcommand\part{\par\addvspace{4ex}\@afterindentfalse \secdef\@part\@spart}

\def\@part[#1]#2{%
  \ifnum \c@secnumdepth >\m@ne
    \refstepcounter{part}%
    \addcontentsline{toc}{part}{\partname\ \thepart: #1}%
  \else
    \addcontentsline{toc}{part}{#1}%
  \fi
  {\parindent \z@ \centering
   \ifnum \c@secnumdepth >\m@ne
     \normalfont\large\rmfamily \MakeUppercase{\partname}\ %
     \ifcase\thepart \or ONE \or TWO \or THREE \or FOUR \or FIVE
     \or SIX \or SEVEN \or EIGHT \or NINE \or TEN \else \fi
     \par \nobreak
   \fi
   \normalfont\LARGE\rmfamily #2 \markboth{}{}\par}%
   \nobreak \vskip 3ex \@afterheading
}

\def\@spart#1{%
  {\parindent \z@ \centering\normalfont\LARGE\rmfamily #1\par}%
  \nobreak \vskip 3ex \@afterheading
}

\newcommand\section{%
  \@startsection {section}{1}{\z@}
    {-23.5\p@ \@plus -6.5\p@ \@minus -3.5\p@}
    {7.5\p@ \@plus \z@ \@minus 1\p@}
    {\normalfont\normalsize\bfseries\centering}%
}

\newcommand\subsection{%
  \@startsection{subsection}{2}{\z@}
    {-19.5\p@ \@plus -3.25\p@ \@minus -3.25\p@}
    {6.5\p@ \@plus \z@ \@minus 1\p@}
    {\normalfont\normalsize\bfseries\itshape\centering}%
}

\newcommand\subsubsection{%
  \@startsection{subsubsection}{3}{\z@}
    {-19.5\p@ \@plus -3.25\p@ \@minus -3.25\p@}
    {6.5\p@ \@plus \z@ \@minus 1\p@}
    {\normalfont\normalsize\itshape\centering}%
}

\newcommand\paragraph{%
  \@startsection{paragraph}{4}{\z@}
    {-13\p@ \@plus -1.5\p@ \@minus -1.5\p@}
    {-0.5em}
    {\normalfont\normalsize\itshape\raggedright}%
}

\newcommand\subparagraph{%
  \@startsection{subparagraph}{4}{\parindent}
    {-13\p@ \@plus -3.25\p@ \@minus -3.25\p@}
    {-0.5em}
    {\normalfont\normalsize\rmfamily\raggedright}%
}

\def\@seccntformat#1{\csname the#1\endcsname\enskip}% FROM LATEX.LTX

\newcommand\appendixname{Appendix}

\def\appnumberline#1{#1\hspace{9pt}}

\newif\ifinappendix

\def\@sect#1#2#3#4#5#6[#7]#8{%
  \ifnum #2>\c@secnumdepth
    \let\@svsec\@empty
  \else
    \refstepcounter{#1}%
    \protected@edef\@svsec{\@seccntformat{#1}\relax}%
  \fi
  \@tempskipa #5\relax
  \ifdim \@tempskipa>\z@
    \begingroup
      #6{%
        \@hangfrom{\hskip #3\relax\@svsec}%
          \interlinepenalty \@M #8\@@par}%
    \endgroup
    \csname #1mark\endcsname{#7}%
  \ifinappendix
    \addcontentsline{toc}{#1}{%
      \ifnum #2>\c@secnumdepth \else
        \protect\appnumberline{\csname the#1\endcsname}%
      \fi
      #7}%
  \else
    \addcontentsline{toc}{#1}{%
      \ifnum #2>\c@secnumdepth \else
        \protect\numberline{\csname the#1\endcsname}%
      \fi
      #7}%
  \fi
  \else
    \def\@svsechd{%
      #6{\hskip #3\relax
      \@svsec #8}%
      \csname #1mark\endcsname{#7}%
      \addcontentsline{toc}{#1}{%
        \ifnum #2>\c@secnumdepth \else
          \protect\numberline{\csname the#1\endcsname}%
        \fi
        #7}}%
  \fi
  \@xsect{#5}}


\newcommand\appendix{\inappendixtrue\par
  \@addtoreset{equation}{section}%
  \@addtoreset{figure}{section}%
  \@addtoreset{table}{section}%
  \setcounter{section}\z@
  \renewcommand\thesection{Appendix~\@Alph\c@section}%
  \renewcommand\thesubsection{\@Alph\c@section.\@arabic\c@subsection}
  \renewcommand\theequation{\@Alph\c@section\@arabic\c@equation}%
  \renewcommand\thefigure{\@Alph\c@section\,\@arabic\c@figure}%
  \renewcommand\thetable{\@Alph\c@section\,\@arabic\c@table}%
}

\setcounter{secnumdepth}{3}

\newcounter{part}
\newcounter{section}
\newcounter{subsection}[section]
\newcounter{subsubsection}[subsection]
\newcounter{paragraph}[subsubsection]
\newcounter{subparagraph}[paragraph]
\renewcommand\thepart          {\@arabic\c@part}
\renewcommand\thesection       {\@arabic\c@section}
\renewcommand\thesubsection    {\thesection.\@arabic\c@subsection}
\renewcommand\thesubsubsection {\thesubsection.\@arabic\c@subsubsection}
\renewcommand\theparagraph     {\thesubsubsection.\@arabic\c@paragraph}
\renewcommand\thesubparagraph  {\theparagraph.\@arabic\c@subparagraph}

\newskip\@leftskip  \@leftskip=\z@

\setlength\leftmargini   {2.5em}
\setlength\leftmarginii  {1.5em}
\setlength\leftmarginiii {1.5em}
\setlength\leftmarginiv  {1.5em}
\setlength\leftmarginv   {1em}
\setlength\leftmarginvi  {1em}
\setlength\leftmargin    {\leftmargini}
\setlength\labelsep      {5\p@}
\setlength\labelwidth    {\leftmargini}
\addtolength\labelwidth  {-\labelsep}

\newcommand\makeRLlabel[1]{\rlap{#1}\hss}
\newcommand\makeRRlabel[1]{\hss\llap{#1}}

\def\@listI{\leftmargin\leftmargini
  \parsep \z@ \topsep 6\p@ %\@plus 3\p@ \@minus 3\p@
  \itemsep \z@ %\@plus 1\p@ \@minus 1\p@
  \let\makelabel\makeRLlabel}

\def\@listii{\leftmargin\leftmarginii
  \labelwidth\leftmarginii
  \advance\labelwidth-\labelsep
  \topsep 3\p@ \@plus 1\p@ \@minus 1\p@
  \parsep \z@ \itemsep \parsep
  \let\makelabel\makeRLlabel}

\def\@listiii{\leftmargin\leftmarginiii
  \labelwidth\leftmarginiii
  \advance\labelwidth-\labelsep
  \topsep 3\p@ \@plus 1\p@ \@minus 1\p@
  \parsep \z@ \partopsep \z@
  \itemsep \topsep
  \let\makelabel\makeRLlabel}

\def\@listiv{\leftmargin\leftmarginiv
  \labelwidth\leftmarginiv
  \advance\labelwidth-\labelsep
  \let\makelabel\makeRLlabel}

\def\@listv{\leftmargin\leftmarginv
  \labelwidth\leftmarginv
  \advance\labelwidth-\labelsep
  \let\makelabel\makeRLlabel}

\def\@listvi{\leftmargin\leftmarginvi
  \labelwidth\leftmarginvi
  \advance\labelwidth-\labelsep
  \let\makelabel\makeRLlabel}

\let\@listi\@listI
\@listi

\def\itemize{% FROM LATEX.LTX
  \ifnum \@itemdepth >\thr@@ \@toodeep\else
     \advance\@itemdepth \@ne
     \edef\@itemitem{labelitem\romannumeral\the\@itemdepth}%
     \ifintheorem\advance\leftmargin\leftmargini\fi
     \expandafter
     \list
       \csname\@itemitem\endcsname
       {\let\makelabel\makeRRlabel}%
  \fi
}

\newcommand\labelitemi{$\m@th\bullet$}
\newcommand\labelitemii{\normalfont\bfseries ---}
\newcommand\labelitemiii{\normalfont\bfseries --}
\newcommand\labelitemiv{$\m@th\cdot$}

\def\enumerate{% FROM LATEX.LTX
  \ifnum \@enumdepth >\thr@@ \@toodeep\else
    \advance\@enumdepth \@ne
    \edef\@enumctr{enum\romannumeral\the\@enumdepth}%
  \fi
  \@ifnextchar [{\@enumeratetwo}{\@enumerateone}%
}

\def\@enumerateone{%
  \expandafter
   \list
     \csname label\@enumctr\endcsname
     {\usecounter{\@enumctr}%
      \ifintheorem\advance\leftmargin\leftmargini\fi
      \let\makelabel\makeRRlabel}%
}

\def\@enumeratetwo[#1]{%
  \expandafter
  \list
    \csname label\@enumctr\endcsname
    {\usecounter{\@enumctr}%
     \settowidth\labelwidth{\normalfont\rmfamily #1}%
     \leftmargin\labelwidth \advance\leftmargin\labelsep
     \let\makelabel\makeRRlabel}%
}

\newcommand\labelenumi  {{\normalfont\rmfamily\theenumi.}}
\newcommand\labelenumii {{\normalfont\rmfamily(\theenumii)}}
\newcommand\labelenumiii{{\normalfont\rmfamily\theenumiii}}
\newcommand\labelenumiv {{\normalfont\rmfamily\theenumiv}}

\renewcommand\theenumi{\@arabic\c@enumi}
\renewcommand\theenumii{\@alph\c@enumii}
\renewcommand\theenumiii{\@roman\c@enumiii}
\renewcommand\theenumiv{\@Alph\c@enumiv}

\renewcommand\p@enumii{\theenumi}
\renewcommand\p@enumiii{\theenumi(\theenumii)}
\renewcommand\p@enumiv{\p@enumiii\theenumiii}

\newcommand*\descriptionlabel[1]{\hspace\labelsep \normalfont\bfseries #1}

\newenvironment{description}
  {\list{}{\leftmargin 1em \labelwidth\z@ \itemindent-\leftmargin
   \let\makelabel\descriptionlabel}}
  {\endlist}

\newenvironment{verse}
  {\let\\=\@centercr
   \list{}{\itemsep\z@
   \itemindent -2.5em%
   \listparindent \itemindent
   \rightmargin\leftmargin
   \advance\leftmargin 2.5em}\item[]}
  {\endlist}

\newenvironment{quotation}
  {\list{}{\listparindent\parindent
   \itemindent\listparindent
   \leftmargin\z@ \rightmargin\leftmargin
   \parsep \z@ \@plus 1\p@}\item[]%
   \normalfont\small\rmfamily}
  {\endlist}

\let\quote\quotation
\let\endquote\endquotation

\newif\ifrembrks
\newcommand\removebrackets{\rembrkstrue}

\newif\ifintheorem

\def\@begintheorem#1#2{% FROM LATEX.LTX
  \intheoremtrue
  \normalfont\rmfamily
  \trivlist
    \pagebreak[3]\item[\hskip \labelsep{\normalfont\itshape #1\ #2}]%
    \item[]%
}

\def\@opargbegintheorem#1#2#3{% FROM LATEX.LTX
  \intheoremtrue
  \normalfont\rmfamily
  \trivlist
    \pagebreak[3]\item[\hskip \labelsep{\normalfont\itshape #1\ #2\ %
    \ifrembrks #3\/\global\rembrksfalse\else {\upshape(}#3\/{\upshape)}\fi}]%
    \item[]%
}

\def\@endtheorem{\endtrivlist\intheoremfalse}

\newsavebox{\proofsavebox}

%\ifprodtf
%  \sbox{\proofsavebox}{$\CUPproofbox$}
%  \newcommand\proofbox{\hbox{$\CUPproofbox$}}
%\else
  \sbox{\proofsavebox}
    {\unitlength 1pt\begin{picture}(6.5,6.5)%
     \put(0,0){\framebox(6.5,6.5){}}\end{picture}}
  \newcommand\proofbox{\usebox{\proofsavebox}\relax}
%\fi

\newcommand\mathproofbox{\rlap{\quad\proofbox}}

\def\@nprf{\normalfont\rmfamily \trivlist
  \pagebreak[3]\item[\hskip \labelsep {\normalfont\itshape Proof}]%
  \item[]}

\def\@oprf[#1]{\normalfont\rmfamily \trivlist
  \pagebreak[3]\item[\hskip \labelsep {\normalfont\itshape #1\ }]%
  \item[]}

\newenvironment{proof}
  {\@ifnextchar[{\@oprf}{\@nprf}}
  {\hspace*{1em}\hbox{\proofbox}\endtrivlist}

\newenvironment{proof*}
  {\proof}
  {\endtrivlist}

\renewcommand\theequation{\@arabic\c@equation}

\setlength\arraycolsep{5\p@}
\setlength\tabcolsep{3\p@}
\setlength\arrayrulewidth{.5\p@}
\setlength\doublerulesep{1.5\p@}
\setlength\tabbingsep{\labelsep}
\setlength{\skip\@mpfootins}{\skip\footins}
\setlength\fboxsep{3\p@}
\setlength\fboxrule{.5\p@}

\newcommand\maketitle{\@ifnextchar [{\m@ketitleone}{\m@ketitleone[n]}}

\def\m@ketitleone[#1]{\par
  \begingroup
    \newpage
    \global\@topnum\z@
    \titlefntrue
    \def\thefootnote{\@fnsymbol\c@footnote}%
    \def\@makefnmark{\hbox{$\@thefnmark$}}%
    \@maketitle{#1}%
    \thispagestyle{titlepage}\@thanks
  \endgroup
  \global\let\@maketitle\relax
  \global\let\@thanks\@empty
  \global\let\@title\@empty
  \global\let\@author\@empty
  \global\let\maketitle\relax
  \global\let\thanks\relax
  \setcounter{footnote}\z@
}

\def\pe@rl#1{%
  \if t#1 {\tpe@rl}\else
    \if T#1 {\Tpe@rl}\else
      \if l#1 {\lpe@rl}\else
        \if L#1 {\Lpe@rl}\else
          \if o#1 {\otherpearl}\else
            \if p#1 {\ppe@rl}\else
              \if P#1 {\Ppe@rl}\else
                \vspace*{32\p@}%
              \fi
            \fi
          \fi
        \fi
      \fi
    \fi
  \fi
}

\def\spe@rl{\vspace*{32\p@}\normalfont\LARGE\rmfamily}
\def\epe@rl#1{\par\vspace*{6.5\p@}\gdef\@shorttitle{#1}}

\def\tpe@rl{\spe@rl T\ls H\ls E\ls O\ls R\ls E\ls T\ls
  I\ls C\ls A\ls L\ns  P\ls E\ls A\ls R\ls L\ls S%
  \epe@rl{Theoretical pearls}%
}

\def\Tpe@rl{\spe@rl T\ls H\ls E\ls O\ls R\ls E\ls T\ls
  I\ls C\ls A\ls L\ns  P\ls E\ls A\ls R\ls L%
  \epe@rl{Theoretical pearl}%
}

\def\lpe@rl{\spe@rl L\ls O\ls G\ls I\ls C\ls A\ls L\ns
  P\ls E\ls A\ls R\ls L\ls S%
  \epe@rl{Logical pearls}%
}

\def\Lpe@rl{\spe@rl L\ls O\ls G\ls I\ls C\ls A\ls L\ns
  P\ls E\ls A\ls R\ls L%
  \epe@rl{Logical pearl}%
}

\def\otherpearl{\spe@rl
  \@ifundefined{othrpearl}
    {Please define {\normalfont\ttfamily\char92 othrpearl} to obtain\\ the correct title!}
    {\othrpearl}%
  \epe@rl{Short title--please redefine with {\normalfont\ttfamily\char92 shorttitle}}%
}


% authors can use p or P to get 'Programming Pearl'
\def\ppe@rl{\spe@rl P\ls R\ls O\ls G\ls R\ls A\ls M\ls M\ls I\ls N\ls G\ns
  P\ls E\ls A\ls R\ls L%
  \epe@rl{Programming pearl}%
}

\def\Ppe@rl{\spe@rl P\ls R\ls O\ls G\ls R\ls A\ls M\ls M\ls I\ls N\ls G\ns
  P\ls E\ls A\ls R\ls L%
  \epe@rl{Programming pearl}%
}

\newcommand\submitted[1]{\gdef\@submitted{#1}}
  \gdef\@submitted{29 September 2017}
\newcommand\revised[1]{\gdef\@revised{#1}}
  \gdef\@revised{20 April 2018}
\newcommand\accepted[1]{\gdef\@accepted{#1}}
  \gdef\@accepted{1 January 2003}

%\AtBeginDocument{
%\def\history{\affilsize\it \ifdefvoid{\@submitted}{}{submitted \@submitted; }\ifdefvoid{\@revised}{}{revised \@revised; }\ifdefvoid{\@accepted}{}{accepted \@accepted}}
%}

\def\b@at{\begin{author@tabular}[t]{@{}c@{}}}

\renewcommand\and{and }
\newcommand\@nd{\end{author@tabular}\vskip 6\p@\par\b@at}

\let\authorbreak\relax
\newcommand\auth@rbreak{\end{author@tabular}\\[0pt]\b@at}

\newbox\authbox
\newenvironment{authgrp}{\global\setbox\authbox\vbox\bgroup\centering}{\egroup}

\newbox\hisbox

\newcommand{\history}[1]{\global\setbox\hisbox\vbox{\affilsize\it\centering #1\@@par}}

\def\sub#1{\textit{submitted\ #1}}
\def\rec#1{\textit{received\ #1}}
\def\rev#1{\textit{revised\ #1}}
\def\acc#1{\textit{accepted\ #1}}

\newbox\titbox

\def\@maketitle#1{%
 \setbox\titbox\vbox{\centering \sloppy
%   \pe@rl{#1}%
\ifOA\vspace*{44.2pt}\else\vspace*{15.5pt}\fi%
\iferratum
{\normalfont\LARGE\itshape{ERRATUM}\@par}
\vspace*{13pt}
\else%
\fi%
   {\normalfont\LARGE\itshape \@title\par}%
   \vskip 12\p@
   {\normalfont\normalsize\rmfamily
    \let\authorbreak\auth@rbreak
    \let\and\@nd
%    \b@at
      \unvbox\authbox%\@author
%    \end{author@tabular}%
   \par}%
%   \ifprodtf
     \vskip 21.5pt%
     \unvbox\hisbox\par
%   \fi
  }\unvbox\titbox%
  \vskip 13\p@ \@plus 2\p@ \@minus 1\p@
}

\def\abs@header#1{%
  \vbox{\hrule \@width\hsize
  \vskip 2.5\p@ \@plus 2\p@ \@minus 1\p@
  \centerline{\normalfont\normalsize\bfseries #1}}%
}

\def\abs@body{%
  \list{}{\leftmargin\z@ \rightmargin\leftmargin
    \listparindent 1em \parsep \z@ \@plus 1\p@
    \topsep 7\p@ \@plus 2\p@ \@minus 1\p@}%
  \item[]\normalfont\small\rmfamily
}

\newcommand\abstractname{Abstract}
\newenvironment{abstract}
  {\abs@header{\abstractname}\abs@body}
  {\endlist
   %\vbox{\hrule \@width \hsize} %rule now included at end of keywords
   \gdef\abs@header##1{\vskip 2\p@
     \centerline{\normalfont\normalsize\bfseries ##1}}
     \ifnokeyword\vskip-4.5pt\vbox{\hrule \@width \hsize}\else\fi}

\newcommand\keywordsname{KEYWORDS}
\newenvironment{keywords}
  {\vspace*{8pt}
  \noindent\normalfont\small\rmfamily{\em \keywordsname}:}%
  {\vspace{4\p@ \@plus 1.5\p@ \@minus 1\p@}\endlist
   \ifnokeyword\else\vbox{\hrule \@width \hsize}\fi}

\newcommand\capsulename{Capsule Review}
\newenvironment{capsule}
  {\abs@header{\capsulename}\abs@body}
  {\endabstract}

\def\author@tabular{\normalfont\authorsize\rmfamily
  \def\@halignto{}\@authortable}
\let\endauthor@tabular\endtabular

\def\author@tabcrone{{\ifnum0=`}\fi\@xtabularcr\normalfont\affilsize\itshape
  \let\\\author@tabcrtwo\ignorespaces}

\def\author@tabcrtwo{{\ifnum0=`}\fi\@xtabularcr[-3\p@]\normalfont\affilsize\itshape
  \let\\\author@tabcrtwo\ignorespaces}

\def\@authortable{\leavevmode \hbox \bgroup $\let\@acol\@tabacol
  \let\@classz\@tabclassz \let\@classiv\@tabclassiv
  \let\\\author@tabcrone \ignorespaces \@tabarray}

\mark{{}{}}
\renewcommand\author{\@ifnextchar [{\@authortwo}{\@authorone}}
\def\@authorone#1{\gdef\@author{#1}\gdef\@shortauthor{#1}}
\def\@authortwo[#1]#2{\gdef\@author{#2}\gdef\@shortauthor{#1}}
\gdef\@author{\mbox{}}

\newcommand\shortauthor[1]{\gdef\@shortauthor{#1}}
\gdef\@shortauthor{}

\renewcommand\title{\@ifnextchar [{\@titletwo}{\@titleone}}
\def\@titleone#1{\gdef\@title{#1}\gdef\@shorttitle{#1}}
\def\@titletwo[#1]#2{\gdef\@title{#2}\gdef\@shorttitle{#1}}
\gdef\@title{\mbox{}}

\newcommand\shorttitle[1]{\gdef\@shorttitle{#1}}
\gdef\@shorttitle{}

\newcommand\volume[1]{\gdef\@volume{#1}}
\gdef\@volume{{\normalfont\bfseries 1} (1)}

\newcommand\pagerange[1]{\gdef\@pagerange{#1}}
\gdef\@pagerange{1 of 000}

\newcommand\jnlDoiYr[1]{\gdef\@year{#1}}
\gdef\@year{20XX}

\newcommand\jyear[1]{\gdef\@jyear{#1}}
\newcommand\fpub[1]{\gdef\@fpub{#1}}

\newcommand\jdate[1]{\gdef\@jdate{#1}}
\gdef\@jdate{January \@year}

\newcommand\doival[1]{\gdef\@doi{\ifonline\href{https://doi.org/#1}{\textcolor{blue}{#1}}\else#1\fi}}
\gdef\@doi{10.1017/S147106840100xxxx}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\AtBeginDocument{}%\pagerange{\pageref*{firstpage}\if@issue--\else\ of \fi\pageref*{lastpage}}
%\RequirePackage{dcolumn}
%\newcolumntype{d}[1]{D{.}{.}{#1}}


%\gdef\@journal{%
%  \vbox to 5.5\p@{\noindent
%    \parbox[t]{4.5in}{\normalfont\indexsize\baselineskip 9\p@
%      {\itshape J.\ Fluid\ Mech.} (\@pubyear), {\itshape vol.} \@volume,
%      {\itshape pp.} \@pagerange.\quad \copyright\ \@pubyear\ Cambridge University
%        Press\\[2.5\p@]
%      DOI: \@doi\quad {Printed in the United Kingdom}}%
%  \vss}%
%}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\newcommand\journal[1]{\gdef\@journal{#1}}

%\def\@journal{%
%  \vbox to 5.6\p@{\noindent\parbox[t]{4.8in}{\normalfont\affilsize\rmfamily
%    {\itshape TLP\/}\ \@volume: \@pagerange, \@jdate.\quad
%    \copyright\ \@year\ Cambridge University Press\\[2.5\p@]%
%    DOI: \@doi\quad Printed in the United Kingdom}%
%  \vss}%
%}

\def\firstpage#1{#1}
\def\lastpage#1{#1}


\def\jnlPage#1#2{\gdef\first@page{\thepage}\gdef\last@page{#2}}

%\def\@journal{%
%  \vbox to 5.6\p@{\noindent\parbox[t]{4.8in}{\normalfont\seven\rmfamily
%    {\itshape TPLP\/}\ \@volume: \@pagerange, \@jdate.\quad
%    \copyright\ Cambridge University Press\ \@year\\[2\p@]%
%    doi:\@doi%\quad Printed in the United Kingdom
%    }%
%  \vss}%
%}

\def\volno#1{\gdef\@volno{#1}}\volno{}%
\def\issno#1{\gdef\@issno{#1}}\issno{}%

\def\cpr#1{\gdef\@cpr{#1}}\cpr{The Author(s), \@year. Published by Cambridge University Press}

\def\@journal{%
  \vbox to 5.6\p@{\noindent\parbox[t]{4.8in}{\normalfont\seven\rmfamily
    {\itshape TPLP}\if@issue\ \textbf{\@volno}\ (\@issno)\/:\ \first@page--\last@page, \@jyear.\else\/: Page \first@page--\last@page.\fi\quad
    \copyright\ \@cpr\ \@year\ifOA.\ \else\\[2\p@]\fi%
    \ifOA\OAtext\vskip1.5pt\else\fi%
    doi:\@doi\if@issue\quad\@fpub\else\fi%\quad Printed in the United Kingdom
    }%
  \vss}%
}


%\def\@underjournal{%
%  \vbox to 5.6\p@{\noindent\parbox[t]{4.8in}{\normalfont\affilsize\rmfamily
%    {\itshape Under consideration for publication in
%     Theory and Practice of Logic Programming\/}\\[2.5\p@]
%     \ }%
%  \vss}%
%}

\def\lefttitle#1{\gdef\@shortauthor{#1}}\lefttitle{}
\def\righttitle#1{\gdef\@shorttitle{#1}}\righttitle{}

\def\ps@headings{\let\@mkboth\markboth
  \def\@oddhead{\hfil{\itshape\@shorttitle}\hfil \llap{\thepage}}%
  \def\@evenhead{\rlap{\thepage}\hfil\itshape\@shortauthor\hfil}%
  \let\@oddfoot\@empty
  \let\@evenfoot\@oddfoot
  \def\sectionmark##1{\markboth{##1}{}}%
  \def\subsectionmark##1{\markright{##1}}%
}

\def\ps@myheadings{\let\@mkboth\@gobbletwo
  \def\@oddhead{\hfil{\itshape\rightmark}\hfil \llap{\thepage}}%
  \def\@evenhead{\rlap{\thepage}\hfil\itshape\leftmark\hfil}%
  \let\@oddfoot\@empty
  \let\@evenfoot\@oddfoot
  \let\sectionmark\@gobble
  \let\subsectionmark\@gobble
}

%\ifprodtf
  \let\@j@urnal\@journal
%\else
%  \let\@j@urnal\@underjournal
%\fi

\def\ps@titlepage{\leftskip\z@ \let\@mkboth\@gobbletwo
  \def\@oddhead{\raisebox{1.3pt}{\@j@urnal \hfil\llap{\thepage}}}%
  \let\@evenhead\@oddhead
  \let\@oddfoot\@empty
  \let\@evenfoot\@oddfoot
  \let\sectionmark\@gobble
  \let\subsectionmark\@gobble
}

%%%%%%%%Yet to update the OA text%%%%%%%%%%%%%%%%%%%

\def\OAtext{This is an Open Access article, distributed under the terms of the Creative Commons Attribution licence (\href{http://creativecommons.org/licenses/by/4.0/}{http://creativecommons.org/licenses/by/4.0/}), which permits
unrestricted re-use, distribution, and reproduction in any medium, provided the original work is properly cited.}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



\def\@pnumwidth{1.55em}
\def\@tocrmarg {2.55em}
\def\@dotsep{4.5}
\setcounter{tocdepth}{2}

\def\@dottedtocline#1#2#3#4#5{% FROM LATEX.LTX
  \ifnum #1>\c@tocdepth \else
    \vskip \z@ \@plus.2\p@
    {\leftskip #2\relax \rightskip \@tocrmarg \parfillskip -\rightskip
     \parindent #2\relax\@afterindenttrue
     \interlinepenalty\@M
     \leavevmode
     \@tempdima #3\relax
     \advance\leftskip \@tempdima \null\hskip -\leftskip
     {#4}\nobreak
     \leaders\hbox{$\m@th
        \mkern \@dotsep mu\hbox{\phantom{.}}\mkern \@dotsep
        mu$}\hfill
     \nobreak
     \hb@xt@\@pnumwidth{\hfil\normalfont \normalcolor #5}%
     \par}%
  \fi}

\newcommand\contentsname{Contents}

\newcommand\tableofcontents{%
  \section*{\contentsname}%
  \@starttoc{toc}\par
  \vspace{13\p@}%
}

\newcommand*\l@part[2]{%
  \ifnum \c@tocdepth >-2\relax
    \addpenalty{-\@highpenalty}%
    \addvspace{.5\baselineskip \@plus 1\p@}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      {\leavevmode
       \normalfont\rmfamily
       #1\hfil \hb@xt@\@pnumwidth{\hfil}}\par
       \nobreak
       \if@compatibility
         \global\@nobreaktrue
         \everypar{\global\@nobreakfalse\everypar{}}%
      \fi
    \endgroup
    \vskip .25\baselineskip \@plus 1\p@
  \fi
}

\newcommand*\l@section[2]{%
  \ifnum \c@tocdepth >\z@
    \addpenalty\@secpenalty
    \setlength\@tempdima{1.5em}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      \leavevmode \normalfont\rmfamily
      \advance\leftskip\@tempdima
      \hskip -\leftskip
      {\bfseries #1}\nobreak\hfil \nobreak\hb@xt@\@pnumwidth{\hss #2}\par
    \endgroup
  \fi}

\newcommand*\l@subsection{\@dottedtocline{2}{1.5em}{2.3em}}
\newcommand*\l@subsubsection{\@dottedtocline{3}{3.8em}{3.2em}}
\newcommand*\l@paragraph{\@dottedtocline{4}{7.0em}{4.1em}}
\newcommand*\l@subparagraph{\@dottedtocline{5}{10em}{5em}}

\newcommand\listfigurename{List of Figures}
\newcommand\listoffigures{%
  \section*{\listfigurename}%
  \@starttoc{lof}\par
  \vskip 13\p@
}

\newcommand*\l@figure{\@dottedtocline{1}{1.5em}{2.3em}}

\newcommand\listtablename{List of Tables}
\newcommand\listoftables{%
  \section*{\listtablename}%
  \@starttoc{lot}\par
  \vskip 13\p@
}

\let\l@table\l@figure

\renewcommand\footnoterule{%
  \kern-3\p@
  \hrule \@width .4\columnwidth height \z@
  \kern 3\p@}

\newif\iftitlefn
\newcommand\@makefntext[1]{%
  \@setpar{\@@par\@tempdima \hsize
  \advance\@tempdima-1em
  \parshape \@ne 1em \@tempdima}\par
  \noindent \hb@xt@ \z@{\hss$\iftitlefn\else^\fi{\@thefnmark}$\ }#1}

%\ifprodtf \else
\let\highast\ast
%\fi

\def\@fnsymbol#1{\ensuremath{% FROM LATEX.LTX
  \ifcase#1\or \hbox{$\highast$}\or \dagger\or \ddagger\or
    \mathchar "278\or \mathchar "27B\or \|\or \hbox{$\highast\highast$}\or
    \dagger\dagger\or \ddagger\ddagger\or \mathchar "278\mathchar "278\or
    \mathchar "27B\mathchar "27B\or \|\|\or \else\@ctrerr\fi}%
}

\renewcommand\@makefnmark{\hbox{$^{\@thefnmark}$}}% FROM LATEX.LTX
\renewcommand\thefootnote{\@arabic\c@footnote}% FROM LATEX.LTX
\renewcommand\thempfootnote{\mathit{\@alph\c@mpfootnote}}% FROM LATEX.LTX

\setcounter{topnumber}{2}
\renewcommand\topfraction{.9}
\setcounter{bottomnumber}{1}
\renewcommand\bottomfraction{.9}
\setcounter{totalnumber}{3}
\renewcommand\textfraction{.1}
\renewcommand\floatpagefraction{.9}
\setcounter{dbltopnumber}{2}
\renewcommand\dbltopfraction{.9}
\renewcommand\dblfloatpagefraction{.5}

\newcounter{table}
\renewcommand\thetable{\@arabic\c@table}
\def\fps@table{tbp}
\def\ftype@table{1}
\def\ext@table{lot}
\newcommand\tablename{Table}
\def\fnum@table{\tablename~\thetable}

\newenvironment{table}
  {\@float{table}}
  {\end@float}

\newenvironment{table*}
  {\@dblfloat{table}}
  {\end@dblfloat}

\def\fstyle@table{\normalfont\small\rmfamily}
\def\fjust@table{}
\def\fcapjust@table{\centering}
\def\fcapsize@table{\normalfont\normalsize\rmfamily}
\def\fcapstyle@table{\normalfont\normalsize\itshape}

\newcommand\contname{(cont.)}
\newcommand\continuedfigure{%
  \addtocounter{figure}\m@ne
  \let\curr@thefigure\thefigure
  \def\thefigure{\curr@thefigure\ \contname}%
}

\newcommand\continuedtable{%
  \addtocounter{table}\m@ne
  \let\curr@thetable\thetable
  \def\thetable{\curr@thetable\ \contname}%
}

\newif\ifrem@fullpt
\newcommand\removefullpoint{\global\rem@fullpttrue}

\newif\ifbot@fig
\newenvironment{bottomfigure}{\def\fps@figure{b}%
  \setcounter{bottomnumber}{1}%
  \global\bot@figtrue
  \@float{figure}\fstyle@figure}
  {\end@float}

\newbox\@tempboxb
\newbox\@tempboxc
\def\FigName{figure}%

\newif\ifmorethantwolines\global\morethantwolinesfalse%


%\long\def\@makecaption#1#2{%
%  \ifbot@fig \rule{\textwidth}{.25\p@}\fi
%  %\vskip 2\p@ \@plus .3\p@ \@minus .2\p@
%  \begingroup
%  \setbox\@tempboxb\hbox{#2}%
%  \def\@xtra{\ifdim\wd\@tempboxb>\z@ \ifrem@fullpt\else .\enskip\fi\fi}%
%  \setbox\@tempboxa\hbox{#1\@xtra #2}%
%  \ifdim\wd\@tempboxa>\tw@\textwidth
%  \ifx\FigName\@captype
%  \ifmorethantwolines%
%      \vspace*{3pt}{\small{#1\@xtra #2\par}}%
%  \else
%    {\vspace*{-3pt}\begin{center}\small #1\@xtra #2\end{center}}\par%
%    \fi
%  \else
%    {#1\@xtra \itshape#2\vspace*{-1pt}}\par%
%  \fi
%  \else
%    \ifx\FigName\@captype
%      \ifmorethantwolines%
%           \vspace*{3pt}{\small{#1\@xtra #2}\par}
%      \else%
%     {\vspace*{-3pt}\begin{center}\small #1\@xtra #2\end{center}}\par
%     \fi
%    \else
%        {#1\@xtra\itshape #2\vspace*{-1pt}}\par
%    \fi
%%    {\fstyle@figure #1\@xtra #2}\par
%%    {\itshape #1\@xtra #2}\par
%  \fi
%  \endgroup
%  \global\bot@figfalse
%  \global\rem@fullptfalse
%}

\newif\iffigcen\global\figcenfalse
\newif\iftabcen\global\tabcenfalse

\def\nocenter{\global\figcentrue}
\def\tabcenter{\global\tabcentrue}

\long\def\@makecaption#1#2{%
  \ifbot@fig \rule{\textwidth}{.25\p@}\fi
  %\vskip 2\p@ \@plus .3\p@ \@minus .2\p@
  \begingroup
  \setbox\@tempboxb\hbox{#2}%
  \def\@xtra{\ifdim\wd\@tempboxb>\z@ \ifrem@fullpt\else .\enskip\fi\fi}%
  \setbox\@tempboxa\hbox{#1\@xtra #2}%
  \setbox\@tempboxc\vbox{#1\@xtra #2}%
  \ifdim\wd\@tempboxa>\tw@\textwidth
  \ifx\FigName\@captype
%  \ifmorethantwolines%
%      \vspace*{3pt}{\small{#1\@xtra #2\par}}%
%  \else
    \iffigcen\else\centering\fi\vspace*{5pt}{\small{#1\@xtra #2\par}}%
%    \fi
  \else
%    {\begin{center}#1\@xtra \itshape#2\end{center}\vspace*{-.5pt}}\par%
\iftabcen%
    {\begin{center}#1\@xtra \itshape#2\end{center}\vspace*{-.5pt}}\par%
    \else
    {#1\@xtra \itshape#2\vspace*{-.5pt}}\par%
  \fi
  \fi
  \else
    \ifx\FigName\@captype
 %     \ifmorethantwolines%
%           \vspace*{3pt}{\small{#1\@xtra #2}\par}
%      \else%
     {\vspace*{-1pt}\begin{center}\small #1\@xtra #2\end{center}}\par
%     \fi
    \else
        {\begin{center}#1\@xtra\itshape #2\end{center}\vspace*{-7pt}}\par
    \fi
%    {\fstyle@figure #1\@xtra #2}\par
%    {\itshape #1\@xtra #2}\par
  \fi
  \endgroup
  \global\bot@figfalse
  \global\rem@fullptfalse
}


\newcounter{figure}
\renewcommand\thefigure{\@arabic\c@figure}
\def\fps@figure{tbp}
\def\ftype@figure{2}
\def\ext@figure{lof}
\newcommand\figurename{Fig.}
\def\fnum@figure{\figurename~\thefigure}

\newenvironment{figure}
  {\@float{figure}}
  {\end@float\global\figcenfalse}

\newenvironment{figure*}
  {\@dblfloat{figure}}
  {\end@dblfloat}

\def\fstyle@figure{\normalfont\small\rmfamily}
\def\fjust@figure{\centering}
\def\fcapjust@figure{\centering}
\def\fcapsize@figure{\normalfont\small\rmfamily}
\def\fcapstyle@figure{\normalfont\small\rmfamily}

\long\def\@caption#1[#2]#3{% FROM LATEX.LTX
  \par
  \addcontentsline{\csname ext@#1\endcsname}{#1}%
    {\protect\numberline{\csname the#1\endcsname}{\ignorespaces #2}}%
  \begingroup
    \@parboxrestore
    \if@minipage
      \@setminipage
    \fi
    \normalsize
    \@makecaption{\csname fcapjust@#1\endcsname
                  \csname fcapsize@#1\endcsname
                  \csname fnum@#1\endcsname}%
                 {\csname fcapstyle@#1\endcsname \ignorespaces #3}\par
  \endgroup
}

\def\@xfloat #1[#2]{% FROM LATEX.LTX
  \@nodocument
  \def\@captype {#1}%
  \def\@fps {#2}%
  \@onelevel@sanitize \@fps
  \def \reserved@b {!}%
  \ifx \reserved@b \@fps
    \@fpsadddefault
  \else
    \ifx \@fps \@empty
      \@fpsadddefault
    \fi
  \fi
  \ifhmode
    \@bsphack
    \@floatpenalty-\@Mii
  \else
    \@floatpenalty-\@Miii
  \fi
  \ifinner
    \@parmoderr\@floatpenalty\z@
  \else
    \@next\@currbox\@freelist
      {%
       \@tempcnta \sixt@@n
       \expandafter \@tfor \expandafter \reserved@a
         \expandafter :\expandafter =\@fps
         \do
          {%
           \if \reserved@a h%
             \ifodd \@tempcnta
             \else
               \advance \@tempcnta \@ne
             \fi
           \fi
           \if \reserved@a t%
             \@setfpsbit \tw@
           \fi
           \if \reserved@a b%
             \@setfpsbit 4%
           \fi
           \if \reserved@a p%
             \@setfpsbit 8%
           \fi
           \if \reserved@a !%
             \ifnum \@tempcnta>15
               \advance\@tempcnta -\sixt@@n\relax
             \fi
           \fi
           }%
       \@tempcntb \csname ftype@\@captype \endcsname
       \multiply \@tempcntb \@xxxii
       \advance \@tempcnta \@tempcntb
       \global \count\@currbox \@tempcnta
       }%
    \@fltovf
  \fi
  \global \setbox\@currbox
    \color@vbox
      \normalcolor
      \vbox \bgroup
        \hsize\columnwidth
        \@parboxrestore
        \@floatboxreset
        \csname fstyle@\@captype\endcsname
        \csname fjust@\@captype\endcsname
}

\let\oldtabular\tabular
\let\endoldtabular\endtabular

\def\tabular{% FROM LATEX.LTX
  \def\@halignto{to \textwidth}\tabskip\tabcolsep \@plus 1fil\@ttabular
}

\def\@ttabular{\leavevmode \hbox \bgroup $\let\@acol\@tabacol
   \let\@classz\@tabclassz
   \let\@classiv\@tabclassiv \let\\\@tabularcr\@ttabarray}

\def\@ttabarray{\m@th\@ifnextchar[\@tarray{\@ttarray}}

\def\@tarray[#1]#2{\t@barray[#1]{@{\tabskip\tw@\tabcolsep \@plus 3\p@}#2}}
\def\@ttarray#1{\t@barray[c]{@{\tabskip\tw@\tabcolsep \@plus 3\p@}#1}}

\def\t@barray[#1]#2{%
  \if #1t\vtop \else \if#1b\vbox \else \vcenter \fi\fi
  \bgroup
  \setbox\@arstrutbox\hbox{%
    \vrule \@height\arraystretch\ht\strutbox
           \@depth\arraystretch \dp\strutbox
           \@width\z@}%
  \@mkpream{#2}%
  \edef\@preamble{%
    \halign \noexpand\@halignto
      \bgroup \tabskip\z@skip \@arstrut \@preamble \tabskip\tabcolsep \@plus 1fil\cr}%
  \let\@startpbox\@@startpbox \let\@endpbox\@@endpbox
  \let\tabularnewline\\%
  \let\par\@empty
  \let\@sharp##%
  \set@typeset@protect
  \lineskip\z@skip\baselineskip\z@skip
  \@preamble}

\newcommand\ls{\kern.15em\relax}
\newcommand\ns{\kern.55em\relax}

\def\hline{% FROM LATEX.LTX
  \noalign{\ifnum0=`}\fi \vskip 6\p@
  \hrule \@height \arrayrulewidth \vskip 6\p@
  \futurelet \reserved@a\@xhline}

\def\@xhline{% FROM LATEX.LTX
  \ifx\reserved@a\hline
    \vskip -12\p@
    \vskip\doublerulesep
  \fi
  \ifnum0=`{\fi}}

\newcommand\today{}
\edef\today{\number\day\ \ifcase\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December
 \fi \ \number\year}

\newenvironment{thebibliography}[1]
     {\section*{\refname}%
      \@mkboth{\MakeUppercase\refname}{\MakeUppercase\refname}%
      \list{\@biblabel{\@arabic\c@enumiv}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \@openbib@code
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
      \sloppy
      \clubpenalty4000
      \@clubpenalty \clubpenalty
      \widowpenalty4000%
      \sfcode`\.\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}

\usepackage[authoryear]{natbib}

\bibpunct{(}{)}{;}{a}{}{;}

\let\citeN\cite
\let\citeANP\citeauthor


\renewcommand\@biblabel[1]{}% FROM LATEX.LTX
%\newcommand\newblock{\hskip .11em \@plus .33em \@minus .07em}
\newcommand\newblock{\relax}

\renewcommand\refname{References}

\newcounter{dummy}
\renewenvironment{thebibliography}[1]
  {\section*{\refname}%
   \normalfont\small\rmfamily
   \addcontentsline{toc}{section}{\refname}%
   \list{}{\usecounter{dummy}\labelwidth\z@ \leftmargin 1em \itemindent -1em}%
   \parindent\z@
   \parskip 2\p@% \@plus .1\p@
   \sloppy\clubpenalty\z@ \widowpenalty\@M
   \sfcode`\.\@m\relax}
  {\def\@noitemerr
   {\@latex@warning{Empty `thebibliography' environment}}%
   \endlist}

%%\def\@citex[#1]#2{% FROM LATEX.LTX
%%  \let\@citea\@empty
%%  \@cite{\@for\@citeb:=#2\do
%%    {\@citea\def\@citea{; }%
%%     \edef\@citeb{\expandafter\@firstofone\@citeb}%
%%     \if@filesw\immediate\write\@auxout{\string\citation{\@citeb}}\fi
%%     \@ifundefined{b@\@citeb}{\mbox{\reset@font\bfseries ?}%
%%       \G@refundefinedtrue
%%       \@latex@warning
%%         {Citation `\@citeb' on page \thepage \space undefined}}%
%%       {\csname b@\@citeb\endcsname}}}{#1}}
%
%\def\@cite#1#2{{\if@tempswa #2\else (#1)\fi}}% FROM LATEX.LTX
%
%\let\@internalcite\cite
%%%\def\cite{\def\citename##1{##1}\@internalcite}
%%%\DeclareRobustCommand\shortcite{\def\citename##1{}\@internalcite}
%
%\def\cite{\def\@citeseppen{-1000}%
%    \def\@cite##1##2{(##1\if@tempswa , ##2\fi)}%
%    \def\citeauthoryear##1##2##3{##2 ##3}\@internalcite}
%\def\citeXNP#1#2{%
%  \def\@cite@label{#2}%
%  \def\@cite##1##2{##1\if@tempswa , ##2\fi}%
%  \def\citeauthoryear##1##2##3{\@cite@label}\@internalcite{#1}}
%% \def\@citeseppen{-1000}%
%%    \def\@cite@label{#1}
%%    \def\@cite##1##2{\if@tempswa ##2\fi}%
%%    \def\citeauthoryear##1##2##3{##2 ##3}\@internalcite}
%\def\citeNP{\def\@citeseppen{-1000}%
%    \def\@cite##1##2{##1\if@tempswa , ##2\fi}%
%    \def\citeauthoryear##1##2##3{##2 ##3}\@internalcite}
%\def\citeN{\def\@citeseppen{-1000}%
%    \def\@cite##1##2{##1\if@tempswa , ##2)\else{)}\fi}%
%    \def\citeauthoryear##1##2##3{##2 (##3}\@citedata}
%\def\shortcite#1{\citeyear{#1}}
%\def\citeS#1{[\citeANP{#1} \citeyearNP{#1}]}
%\def\citeNS#1{\citeANP{#1} \citeyear{#1}}
%\def\citeNPS#1{\citeANP{#1} \citeyearNP{#1}}
%%testing year,year
%\def\citeNN{\def\@citeseppen{-1000}%
%    \def\@cite##1##2{(##1\if@tempswa , ##2\fi)}%
%    \def\citeauthoryear##1##2##3{##3}\@citedata}
%
%\def\citeA{\def\@citeseppen{-1000}%
%    \def\@cite##1##2{(##1\if@tempswa , ##2\fi)}%
%    \def\citeauthoryear##1##2##3{##2}\@internalcite}
%\def\citeANP{\def\@citeseppen{-1000}%
%    \def\@cite##1##2{##1\if@tempswa , ##2\fi}%
%    \def\citeauthoryear##1##2##3{##2}\@internalcite}
%
%\def\citeyear{\def\@citeseppen{-1000}%
%    \def\@cite##1##2{(##1\if@tempswa , ##2\fi)}%
%    \def\citeauthoryear##1##2##3{##3}\@citedata}
%\def\citeyearNP{\def\@citeseppen{-1000}%
%    \def\@cite##1##2{##1\if@tempswa , ##2\fi}%
%    \def\citeauthoryear##1##2##3{##3}\@citedata}
%
%%
%% \@citedata and \@citedatax:
%%
%% Place commas in-between citations in the same \citeyear, \citeyearNP,
%% or \citeN command.
%% Use something like \citeN{ref1,ref2,ref3} and \citeN{ref4} for a list.
%%
%\def\@citedata{%
%        \@ifnextchar [{\@tempswatrue\@citedatax}%
%                                  {\@tempswafalse\@citedatax[]}%
%}
%
%\def\@citedatax[#1]#2{%
%\if@filesw\immediate\write\@auxout{\string\citation{#2}}\fi%
%  \def\@citea{}\@cite{\@for\@citeb:=#2\do%
%    {\@citea\def\@citea{; }\@ifundefined% by Young
%       {b@\@citeb}{{\bf ?}%
%       \@warning{Citation `\@citeb' on page \thepage \space undefined}}%
%{\csname b@\@citeb\endcsname}}}{#1}}%
%
%% don't box citations, separate with ; and a space
%% also, make the penalty between citations negative: a good place to break.
%%
%\def\@citex[#1]#2{%
%\if@filesw\immediate\write\@auxout{\string\citation{#2}}\fi%
%  \def\@citea{}\@cite{\@for\@citeb:=#2\do%
%    {\@citea\def\@citea{; }\@ifundefined% by Young
%       {b@\@citeb}{{\bf ?}%
%       \@warning{Citation `\@citeb' on page \thepage \space undefined}}%
%{\csname b@\@citeb\endcsname}}}{#1}}%



\newif\iffontfound
\newcommand\checkfont[1]{%
  \batchmode
  \font\test=#1\relax
  \errorstopmode
  \fontfoundfalse
  \ifx\test\nullfont \else \fontfoundtrue\fi
}

\newcommand\email[2][e-mail]{{\normalfont\rmfamily
  \itshape\textup{(}#1: \textup{\texttt{\href{mailto:#2}{\color{blue}#2}})}}}

\def\emails#1#2{{\normalfont\rmfamily
  \itshape\textup{(}e-mails: \textup{\texttt{\href{mailto:#1}{\color{blue}#1},}}\ \textup{\texttt{\href{mailto:#2}{\color{blue}#2}})}}}


\edef\r@{%\ifprodtf mtr\else
cmr%
%\fi
}

\def\linkref#1#2{\iffinals#2\else\textcolor{blue}{\hyperlink{#1}{#2}}\fi}
\def\linktarget#1#2{\iffinals#2\else\hypertarget{#1}{#2}\fi}

\usepackage{marginnote}

\clubpenalty10000\widowpenalty10000\hyphenpenalty=50%

\def\endash{--}

\ifonline
\def\orcidau[#1]#2{\href{https://orcid.org/#1}{#2}}
\else
\def\orcidau[#1]#2{#2}
\fi

\newcommand{\orcid}[1][]{\iffinals\includegraphics{CUP_orcid_logo_Print}\else\href{https://orcid.org/#1}{\includegraphics{CUP_orcid_logo_Web}}\fi}

\ifprinter
   \def\draftnote{\vphantom{\vbox to 0pt{\vskip-12pt}
   \hbox{\ \footnotesize\jobname\vphantom{pq}\qquad\jobtag\qquad\today\qquad\currenttime\qquad Char Count= \@TotalChars
   \ifmathtif\else\@questionmark\fi\hfil}}}%
  \def\comment{}
  \def\Qauthor#1{}
   \def\QtoAuthor#1{}
   \def\QtoClient#1{}
   \def\QtoQC#1{}
   \def\QtoGL#1{}
   \def\qtoa#1{}
\else
\def\draftnote{\vbox to 0pt{\vskip-12pt}}%
   \def\Qauthor#1{\marginpar{{\raggedright\footnotesize\bf #1\endgraf}}}
\fi
\let\qtoa\Qauthor

\def\gray{\special{color push cmyk 0 0 0 0.5}}
\def\@questionmark{\vbox to 0pt{\rotatebox{40}{\hbox to \textwidth{
                  \begin{picture}(0,0)\put(-340,-50){
                  \fontsize{90}{90}\selectfont Compile Again}\end{picture}\hss}}}}
\def\questionmark#1{\xdef\@questionmark{#1}}%
%
\def\@TotalChars{}
\def\TotalChars#1{\xdef\@TotalChars{#1}}%


\ifonline
\usepackage{hyperref}%
\usepackage[all]{hypcap}
\hypersetup{
draft=false,
dvips,
colorlinks=true,
linkcolor=blue,
anchorcolor=blue,
citecolor=blue,
urlcolor=blue,
filecolor=blue,
bookmarksopenlevel=3,
bookmarksopen=true,
pdfborderstyle={/S/U/W 0},
pdfstartview={FitH 800},
breaklinks=true}
\usepackage{breakurl}
\urlstyle{tt}
\usepackage{bookmark}[2011/12/02]
\usepackage{etoolbox}
\else
\fi


\let\real@font@warning\@font@warning
\DeclareMathVersion{program}
\let\@font@warning\@gobble
\SetSymbolFont{letters}{program}{OT1}{\r@}{m}{sl}
\let\@font@warning\real@font@warning
\SetMathAlphabet{\mathnormal}{program}{OT1}{\r@}{m}{sl}

\newcommand{\programmath}{\mathversion{program}}
\newcommand{\unprogrammath}{\mathversion{normal}}
\newcommand{\figrule}{\begin{center}\hrule\end{center}}
%\newcommand{\topfigrule}{\begin{center}\hrule\end{center}}
%\newcommand{\bfigrule}{\begin{center}\hrule\end{center}}

\newcommand{\bfigrule}{
\figrule
\@par\addvspace{-8.7pt}
}

\newcommand{\tfigrule}{
\figrule
\vspace*{-11pt}
}

\DeclareRobustCommand\dplus{\mathbin{+\!\!+}}
\DeclareRobustCommand\dequals{\mathbin{==}}
\DeclareRobustCommand\dcolon{\mathbin{::}}
\DeclareRobustCommand\dcolonequals{\mathbin{::=}}


\pagestyle{headings}
\pagenumbering{arabic}
\frenchspacing
\sloppy
%\flushbottom

\AtEndDocument{\label{lastpage}}

\usepackage[inline]{enumitem}

%\newdimen\myb
%\myb
%\myb

\raggedbottom

\def\authorfont{\fontsize{10}{12}\selectfont}
\def\affilfont{\fontsize{8}{10}\selectfont\itshape}

\def\author#1{\vskip7pt{#1\authorfont\vskip0pt}}
\def\affiliation#1{{\affilfont#1\vskip0pt}}
\def\sn#1{\MakeUppercase{#1}}
\def\gn#1{\MakeUppercase{#1}}

\newenvironment{arabiclist}{%
%\abovelistskipi6\p@ plus2pt
%\belowlistskipi6\p@ plus2pt
%\labelsepi5pt
\setlength\leftmargini{18pt}
\def\theenumi{\arabic{enumi}}
\def\theenumii{\alpha{enumii}}%
\def\theenumiii{\arabic{enumiii}}%
\def\theenumiv{\arabic{enumiv}}%
\def\theenumv{\arabic{enumv}}%
\def\theenumvi{\arabic{enumvi}}%
\def\theenumvii{\arabic{enumvii}}%
\def\theenumviii{\arabic{enumviii}}%
\def\theenumix{\arabic{enumix}}%
\def\theenumx{\arabic{enumx}}%
\renewcommand\labelenumi{\theenumi.}
\renewcommand\labelenumii{\theenumii.}
\renewcommand\labelenumiii{\theenumiii.}
  \begin{enumerate}}
  {\end{enumerate}}


\newskip\tabnoteleftskip%
\def\tabnotefont{\fontsize{8}{10}\selectfont}
\newenvironment{tabnote}{\par\tabnotefont}
    {\par}


%%%%%%%%Author Color%%%%%%%%
\definecolor{snamecol}{rgb}{1,.64,1}

\def\LDAUTH#1{\ifnoquery{#1}\else\fboxsep0pt\fboxrule0pt\fcolorbox{snamecol}{snamecol}{#1}\fi}%

\def\midrule{\\[-10.5pt]\hline\hline\\[-13.5pt]}
\def\botrule{\\[-15pt]\hline\hline\\[-13.5pt]}

\def\thanks#1{\textsuperscript{\footnotemark}
    \protected@xdef\@thanks{\@thanks
        \protect\thankstext[\the\c@footnote]{#1}}}

\def\thankstext{%
     \@ifnextchar [\@thankstext
       {\protected@xdef\@thefnmark{\thempfn}%
    \@footnotetext}}

\def\@thankstext[#1]{%
  \begingroup
     \csname c@\@mpfn\endcsname #1\relax
     \unrestored@protected@xdef\@thefnmark{\textsuperscript{\thempfn}}%
  \endgroup
  \@footnotetext}

\newcounter{tbnlink}
\def\thetbnlink{\arabic{tbnlink}}
\def\tbnlreffont{}
\def\tnlref#1{\ifonline\textcolor{blue}{\hyperlink{tbn-\thetbnlink}{\hbox{\tbnlreffont#1}}}\else\tbnlreffont#1\fi}
\def\tnlget#1{\ifonline\hypertarget{tbn-\thetbnlink}{\scriptscriptstyle#1}\else#1\fi}

\def\doilink#1{\ifonline\href{https://doi.org/#1}{\textcolor{blue}{#1}}\else#1\fi}
\def\arxlink#1{\ifonline\href{https://arxiv.org/abs/#1}{\textcolor{blue}{#1}}\else#1\fi}
\def\emaillink#1{\ifonline\href{mailto:#1}{\textcolor{blue}{#1}}\else#1\fi}
\def\httplink#1{\ifonline\href{#1}{\textcolor{blue}{\refuri #1}}\else{\refuri #1}\fi}

\newcounter{bibetem}
\def\thebibetem{\arabic{bibetem}}

\def\bibitem@fin{\refstepcounter{bibetem}%
 \@ifxundefined\@bibstop{}{\csname bibitem@\@bibstop\endcsname}%
}%

\newcommand{\citeb}[2][]{\hyperref[#1]{#2}}

\newenvironment{unnumlist}{%
\setlength\leftmargini{-6pt}
%\itemindent-\leftmargini
\def\theenumi{\arabic{enumi}}
\def\theenumii{\alpha{enumii}}%
\def\theenumiii{\arabic{enumiii}}%
\def\theenumiv{\arabic{enumiv}}%
\def\theenumv{\arabic{enumv}}%
\def\theenumvi{\arabic{enumvi}}%
\def\theenumvii{\arabic{enumvii}}%
\def\theenumviii{\arabic{enumviii}}%
\def\theenumix{\arabic{enumix}}%
\def\theenumx{\arabic{enumx}}%
\renewcommand\labelenumi{\theenumi.}
\renewcommand\labelenumii{\theenumii.}
\renewcommand\labelenumiii{\theenumiii.}
  \begin{itemize}}
  {\end{itemize}}

\def\scboldfont{\fontsize{7.5}{7.5}\selectfont\bfseries}
\def\tablefont{\fontsize{9}{11}\selectfont\tabcolsep0pt}
\def\journaltitle#1{\gdef\journal@title{#1}}%

\def\topline{\hline\\[-21pt]\hline}
\def\midline{\\[-2pt]\hline}
\def\botline{\\[-2pt]\hline\\[-21pt]\hline}

\endinput
