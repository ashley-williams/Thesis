%Time-stamp: "Last modified: 2019-01-25 11:27:10 (d_yasaki)"
% indent every paragraph
%fixed spacing in toc for bibliography and appendix.  07272018
% Fixed vertical spacing on Bibliography page 0802018

\ProvidesClass{uncgdissertationexp}[2018/04/23 Experimental LaTeX
  class for UNCG dissertation]
\NeedsTeXFormat{LaTeX2e}

\LoadClass[oneside, openany, 12pt, onecolumn, final]{book}
\RequirePackage{graphicx} % maybe fix?
\RequirePackage{fancyhdr} %% Don't know what this does.
\RequirePackage[tableposition=top, labelsep = period]{caption}
\RequirePackage[section, below]{placeins}
\RequirePackage{etoolbox}
\RequirePackage{calc}
\RequirePackage{titlesec}
\RequirePackage[dotinlabels, rightlabels]{titletoc}
\RequirePackage{ifthen}
\RequirePackage{lastpage}  % to count pages
\RequirePackage{setspace}  % to do doublespace
\RequirePackage[overload]{textcase} % So that math does not get
                                % capitalized in
                          % chapter titles.  Note that the overload
                          % option is used, so this redefines
                          % \MakeUppercase
\RequirePackage[noadjust]{cite}
\RequirePackage{indentfirst}
\RequirePackage[T1]{fontenc}
\RequirePackage[top = 1.5in, right = 1in, left =
  1.5in]{geometry}  % to set margins
\newif\if@MA  % Masters MA thesis format
\newif\if@MS  % Masters MS thesis format
\newif\if@PHD  % PhD dissertation format

\newcommand{\uncg@thesistype}{phd}
\DeclareOption{phd}{\renewcommand{\uncg@thesistype}{phd} \@MAfalse
  \@MSfalse \@PHDtrue}
\DeclareOption{ma}{\renewcommand{\uncg@thesistype}{ma} \@MAtrue
  \@MSfalse \@PHDfalse}
\DeclareOption{ms}{\renewcommand{\uncg@thesistype}{ms} \@MStrue
  \@MAfalse \@PHDfalse}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{book}}
\ProcessOptions\relax

\contentsmargin[4em]{18pt}  % sets how far the multiline section
                            % titles can come over and also the
                            % distance the leaders stay from the page
                            % numbers.

%-----------------------------------------------------------------------------
% Count the number of co-chairs
\newcounter{@chair}           % Number of chairs defined so far.
\setcounter{@chair}{0}
\newcommand{\chair}[1]{
  \addtocounter{@chair}{1}    % Inc. num of classes to include this
                              % new one.
  \ifcase\value{@chair}
   \or \newcommand{\uncg@chair}{#1}
    \or \newcommand{\uncg@cochair}{#1}
  \else
     \ClassError{uncgdissertation.cls}
        {Committee error.  Too many chairs.}
  \fi
}
%-----------------------------------------------------------------------------
%-----------------------------------------------------------------------------
% Count the number of members (not including chair)
\newcounter{@member}           % Number of members defined so far.
\setcounter{@member}{0}
\newcommand{\member}[1]{
  \addtocounter{@member}{1}    % Inc. num of classes to include this
                               % new one.
  \ifcase\value{@member}
   \or \newcommand{\uncg@memberI}{#1}
    \or \newcommand{\uncg@memberII}{#1}
     \or \newcommand{\uncg@memberIII}{#1}
      \or \newcommand{\uncg@memberIV}{#1}
       \or \newcommand{\uncg@memberV}{#1}
  \else
     \ClassError{uncgdissertation.cls}
        {Committee error.  Too many members.}
  \fi
}
%-----------------------------------------------------------------------------

%% -------------------------------------------------------------------------- %%
%% Thesis Title
\renewcommand{\title}[1]{\newcommand{\uncg@thesistitle}{#1}}


\newcommand{\student}[2]{
    \newcommand{\uncg@authorfirstmiddle}{#1}
    \newcommand{\uncg@authorlast}{#2}
    \newcommand{\uncg@author}{#1 #2}
    \author{#1 #2}
}

\if@MA{\gdef\@thesis{thesis}
  \gdef\@capthesis{Thesis}
  \gdef\@degree{Master~of~Arts}
  \gdef\@shortdegree{M.A.}} \else
\if@MS{\gdef\@thesis{thesis}
  \gdef\@capthesis{Thesis}
  \gdef\@degree{Master~of~Science}
  \gdef\@shortdegree{M.S.}} \else
\if@PHD{\gdef\@thesis{dissertation}
  \gdef\@capthesis{Dissertation}
  \gdef\@degree{Doctor~of~Philosophy}
  \gdef\@shortdegree{Ph.D.}} \fi\fi\fi



%% Degree year
\newcommand{\degreeyear}[1]{
    \newcommand{\uncg@year}{#1}
}

\newenvironment{abstract}{%
  \singlespacing
  \thispagestyle{empty}
  \setcounter{page}{0}
  \noindent
  \MakeUppercase{\uncg@authorlast{},~%
    \uncg@authorfirstmiddle{}},~\@shortdegree~\uncg@thesistitle. (\uncg@year)\\
  Directed by
  \ifthenelse{\value{@chair}=1}{Dr.~\uncg@chair}{Dr.~\uncg@chair\ and
    Dr.~\uncg@cochair}. \pageref{LastPage} pp.\\
  %line break above needed to make above singlespace and below
  %doublespace

  \doublespacing
  \pagestyle{empty}
}%
{\clearpage
\pagestyle{plain}
}


% Length of signature lines and height
\newlength{\namewidth}
\setlength{\namewidth}{2.8in} % Hard-coded to align start of signature
                            % with first o in Greensboro

%% Line height
\newlength{\uncg@signatureheight}
\setlength{\uncg@signatureheight}{2em}


\newcommand{\uncg@signatureline}[1]{
    \begin{minipage}[t]{\namewidth}
      \underline{\hspace*{\namewidth}}
     \vspace{0.5ex}
     #1
    \end{minipage}
}


\newcommand{\uncg@dateline}[1]{
    \begin{minipage}[t]{\namewidth}
      %\underline{\hspace*{\namewidth}}\\
      \underline{\phantom{#1}}\par
     \vspace{0.5ex}
     #1
    \end{minipage}
}


% This should be switched if more than one chair.
\newcommand{\uncg@chairsignature}{
  \begin{minipage}[t]{\namewidth}
    \ifthenelse{\value{@chair}=1}{
      Approved by\\[\uncg@signatureheight]
      \uncg@signatureline{Committee Chair}
      }{
      Approved by\\[\uncg@signatureheight]
      \uncg@signatureline{Committee Co-Chair}\\[\uncg@signatureheight]
      \uncg@signatureline{Committee Co-Chair}}
  \end{minipage}
}


\newcommand{\uncg@datelines}{
  \begin{minipage}{\namewidth}
    \uncg@dateline{Date of Acceptance by
      Committee}\\[\uncg@signatureheight]
    \uncg@dateline{Date of Final Oral Examination}\\
  \end{minipage}
}

\newcommand{\uncg@chairtable}
{
\ifthenelse{\value{@chair}=1}{
Committee Chair &
\uncg@signatureline{\uncg@chair}\\[\uncg@signatureheight]}
{
Committee Co-Chair &
\uncg@signatureline{\uncg@chair}\\[\uncg@signatureheight]
Committee Co-Chair &
\uncg@signatureline{\uncg@cochair}\\[\uncg@signatureheight]
}
}

\newcommand{\uncg@committeesignatures}{
  \ifthenelse{\value{@member}=2}{
%Come back
    \begin{tabular}[t]{r@{\hspace{2.7pt}}l}%c@{\hspace{0.5in}}c}
      \uncg@chairtable
      Committee Members
      &\uncg@signatureline{\uncg@memberI}\\[\uncg@signatureheight]
       &\uncg@signatureline{\uncg@memberII}
    \end{tabular}
}{} % end ifthenelse = 3

  \ifthenelse{\value{@member}=3}{
    \begin{tabular}[t]{r@{\hspace{2.7pt}}l}%c@{\hspace{0.5in}}c}
\uncg@chairtable
      Committee Members
      &\uncg@signatureline{\uncg@memberI}\\[\uncg@signatureheight]
       &\uncg@signatureline{\uncg@memberII}\\[\uncg@signatureheight]
     &\uncg@signatureline{\uncg@memberIII}
    \end{tabular}
  }{} % end ifthenelse = 4

\ifthenelse{\value{@member}=4}{
\begin{tabular}[t]{r@{\hspace{2.7pt}}l}%c@{\hspace{0.5in}}c}
\uncg@chairtable
      Committee Members
      &\uncg@signatureline{\uncg@memberI}\\[\uncg@signatureheight]
       &\uncg@signatureline{\uncg@memberII}\\[\uncg@signatureheight]
     &\uncg@signatureline{\uncg@memberIII}\\[\uncg@signatureheight]
     &\uncg@signatureline{\uncg@memberIV}
    \end{tabular}
}{} % end ifthenelse = 5
  \ifthenelse{\value{@member}=5}{
\begin{tabular}[t]{r@{\hspace{2.7pt}}l}%c@{\hspace{0.5in}}c}
\uncg@chairtable
      Committee Members
      &\uncg@signatureline{\uncg@memberI}\\[\uncg@signatureheight]
       &\uncg@signatureline{\uncg@memberII}\\[\uncg@signatureheight]
     &\uncg@signatureline{\uncg@memberIII}\\[\uncg@signatureheight]
     &\uncg@signatureline{\uncg@memberIV}\\[\uncg@signatureheight]
      &\uncg@signatureline{\uncg@memberV}
    \end{tabular}
}{} % end ifthenelse = 6
}

%% Title page
\newcommand{\maketitlepage}{
  \thispagestyle{empty}  % after chapter* command to ensure no page
                         % numbers.
{\doublespacing
\centering
    \vspace*{0.5\baselineskip}
  \MakeUppercase{\uncg@thesistitle}
\vspace*{0.5\baselineskip}

    by \par
    \vspace*{0.5\baselineskip}

    \uncg@author \par
    \vspace*{2\baselineskip}

    \singlespacing
   A \@capthesis\ Submitted to \\
  the Faculty of The Graduate School at\\
  The University of North Carolina at Greensboro\\
  in Partial Fulfillment\\
  of the Requirements for the Degree\\
  \@degree\\
}
    \vfill
{\centering
    Greensboro\\
    \uncg@year\par
  }
  \vfill

  \begin{flushright}    \uncg@chairsignature \end{flushright}
  \clearpage
  \doublespacing
}

%% Copyright page
\newcommand{\makecopyrightpage}{ \addtocounter{page}{-1}
  \thispagestyle{empty} \ \vfill
      \begin{center}
        \copyright{} \uncg@year{} \uncg@author
      \end{center}
      \ \vfill
\clearpage
}


%%  Dedication page
\newenvironment{dedication}
{\setcounter{page}{2} ~\vfill \par \centering \sl}
{ \vfill \par \clearpage}

%%  Acknowledgments page
\newenvironment{acknowledgments}
{\shortchapter*{Acknowledgments}\par
\vspace*{0.5\baselineskip}  }
{ \vfill \par \clearpage}


%%  Preface page
\newenvironment{preface}
{\shortchapter*{Preface}\par
\vspace*{0.5\baselineskip}  }
{ \vfill \par \clearpage}

%% Approval page
\newcommand{\makeapprovalpage}{
\shortchapter*{APPROVAL PAGE}
\vspace*{0.5\baselineskip}

  This \@thesis\ written by \uncg@author\ has been approved by the
  following committee of the Faculty of The Graduate School at The
  University of North Carolina at Greensboro.
  \vfill
  \singlespacing
  \begin{center}
    \uncg@committeesignatures
  \end{center}
  \vfill
  \begin{flushleft}
    \uncg@datelines
  \end{flushleft}
  \vfill
  \doublespacing
  \clearpage
}

\titleformat{\chapter}[display]{\centering}{\MakeUppercase{\chaptername}\ \thechapter}{0pt} {\uppercase
} % before-code
% \titlespacing* to make sure spacing is correct on top of pages also.
\titlespacing*{\chapter}{0pt}{-24pt + 0.5in}{12pt}
% need 12pt (.5 \baselineskip) in last to make triple space before text
% need -\baselineskip to make the margin 1.5 in.  Then increase space
% by 0.5 in.

%% create a new chapter type to account for required 1.5 inch margins
%% on certain pages

%-------------------------------------------------------------------------
\titleclass{\shortchapter}{top}[\chapter]
\newcounter{shortchapter}
\renewcommand{\theshortchapter}{\arabic{shortchapter}}
%-------------------------------------------------------------------------

%-------------------------------------------------------------------------

\titleformat{\shortchapter}[display]{\centering}{~\vspace*{-24pt}}{0pt}{\uppercase}[]
\titlespacing*{\shortchapter}{0pt}{-24pt}{0pt}
% need 12pt (.5 \baselineskip) in last to make triple space before
% text, but this is added to each page that uses it instead.  e.g.,
% acknowledgment, preface.  This is to make the TOC easier to debug.
% need -24pt to make the margin 1.5 in.  Then increase space
% by 0.5 in.

% add and subtract space in format to get the right space when
% section/ or subsection starts a page.
\titleformat{\section}[hang] {\vspace{0.85cm}\vspace*{-0.85cm}\bf}{\thesection. }{0pt}{}
% \titlespacing* to make sure spacing is correct on top of pages also.
\titlespacing*{\section}{0pt}{0pt \@plus1ex \@minus.2ex}{0pt\@plus.2ex}
%\titlespacing*{\section}{0pt}{0*}{0*}
% remove spacing before and after

\titleformat{\subsection}[hang] {\vspace{0.85cm}\vspace*{-0.85cm}\it}{\thesubsection. }{0pt}{}

\titlespacing*{\subsection}{0pt}{0pt \@plus1ex \@minus.2ex}{0pt \@plus.2ex}
%\titlespacing*{\subsection}{0pt}{0*}{0*}
% remove spacing before and after
%-------------------------------------------------------------------------
\setlength{\parindent}{0.5in}
\g@addto@macro\normalsize{%
  %  need 7.27pt since 12/\baselinestretch = 7.27pt
  % commented out what they would be using the conversion.
  % make the plus minus a bit smaller.
  %\setlength{\abovedisplayskip}{7.27\p@ \@plus1.8\p@ \@minus4.2\p@}%
  %\setlength{\belowdisplayskip}{7.27\p@ \@plus1.8\p@ \@minus4.2\p@}%
  %\setlength{\abovedisplayshortskip}{\z@ \@plus1.8\p@}%
  %\setlength{\belowdisplayshortskip}{3.9\p@ \@plus2.1\p@
  %  \@minus1.8\p@}%
  \setlength{\abovedisplayskip}{7.27\p@ \@plus1\p@ \@minus1\p@}%
  \setlength{\belowdisplayskip}{7.27\p@ \@plus1\p@ \@minus1\p@}%
  \setlength{\abovedisplayshortskip}{\z@ \@plus1\p@}%
  \setlength{\belowdisplayshortskip}{3.9\p@ \@plus1\p@ \@minus1\p@}%
}
% if a figure is alone on a page, move it to the very top
\setlength{\@fptop}{0pt}

\g@addto@macro\listoffigures{%
  \addtocontents{lof}{~\hfill Page\par}
  \addtocontents{lof}{\vspace*{-5pt}}
}
\g@addto@macro\tableofcontents{%
\addtocontents{toc}{~\hfill Page\par }
\addtocontents{toc}{\vspace*{-5pt}}
}
\g@addto@macro\listoftables{%
  \addtocontents{lot}{~\hfill Page\par}
  \addtocontents{lot}{\vspace*{-5pt}}
}

\newcommand{\mychaptercounter}{0}
\newcommand{\myappcounter}{0}

\g@addto@macro\mainmatter{%
\addtocontents{toc}{\vspace*{5pt}}
\addtocontents{toc}{\noindent CHAPTER\par}
\addtocontents{toc}{\vspace*{-5pt}}
}


\AtBeginEnvironment{table}{\vspace{16pt}}
\AtBeginEnvironment{figure}{\addvspace{12pt}}


%-------------------------------------------------------------------------
%\setlength\floatsep{0.0pt \@plus 0.0pt \@minus 0.0pt}%
%\setlength\textfloatsep{20.0pt \@plus 0.0pt \@minus 0.0pt}%
% not enough space
%\setlength{\intextsep}{10pt \@plus 4.0pt \@minus 4.0pt}%
% too much space
%\setlength{\intextsep}{20pt \@plus 4.0pt \@minus 4.0pt}%
% too little space
%\setlength{\intextsep}{13pt \@plus 2.0pt \@minus 2.0pt}%
\setlength{\intextsep}{16pt \@plus 2.0pt \@minus 2.0pt}%
%\setlength{\textfloatsep}{6pt \@plus 1.0pt \@minus 1.0pt}
%\textfloatsep â€” distance between floats on the top or the bottom and
%the text;
%\floatsep â€” distance between two floats;
%\intextsep â€” distance between floats inserted inside the page text
%(using h) and the text proper.

\renewcommand{\bibname}{\par BIBLIOGRAPHY }
\pagestyle{plain}
\AtBeginEnvironment{thebibliography}{\singlespacing}
% \patchcmd{<cmd>}{<search>}{<replace>}{<success>}{<failure>}
\patchcmd{\listoftables}{\chapter*{\listtablename}}{\shortchapter{LIST
    OF TABLES}}{}{}
\patchcmd{\listoffigures}{\chapter*{\listfigurename}}{\shortchapter{LIST
  OF FIGURES}}{}{}
% fix vertical spacing on bibliography page
\pretocmd{\thebibliography}{\titlespacing{\chapter}{0pt}{20pt}{24pt}}{}{}
\apptocmd{\thebibliography}{\csname
  phantomsection\endcsname\addcontentsline{toc}{shortchapter}{\bibname}\addtocontents{toc}{\protect\addvspace{-10\p@}}}
{}{}


\patchcmd{\tableofcontents}{\chapter*{\contentsname \@mkboth{%
           \MakeUppercase\contentsname}{\MakeUppercase\contentsname}}}{\shortchapter*{TABLE OF CONTENTS\@mkboth{
           TABLE OF CONTENTS}{TABLE OF CONTENTS}}}{}{}

\patchcmd{\@chapter}{\addtocontents{lof}{\protect\addvspace{10\p@}}}{}{}{}
\patchcmd{\@chapter}{\addtocontents{lot}{\protect\addvspace{10\p@}}}{}{}{}

%-------------------------------------------------------------------------
\def\ttl@tocsep{}  % get rid of extra space introduced in List of
                   % Figures and List of Tables
\setcounter{tocdepth}{2}  % only show chapters and sections in TOC
\setcounter{secnumdepth}{2}
\renewcommand{\thechapter}{\Roman{chapter}}
\renewcommand{\thefigure}{\arabic{chapter}.\arabic{figure}}
\renewcommand{\thetable}{\arabic{chapter}.\arabic{table}}
\patchcmd{\@chapter}{#1}{\MakeUppercase{#1}}{}{}

\titlecontents{shortchapter}[0pt] {\addvspace{10pt}
\singlespacing}{}{}{\titlerule*[6pt]{.}\contentspage}[\vspace*{-10pt}]
\titlecontents{shortchapter}[0pt] {\singlespacing}{}{}{\titlerule*[6pt]{.}\contentspage}[\vspace*{-5pt}]

\titlecontents{chapter}[.85in]
              {\addvspace{10pt} \singlespacing\hspace*{-.2in}}{\contentslabel{.5in}}{\hspace*{-.5in}}{\titlerule*[6pt]{.}\contentspage}[\vspace*{-5pt}]

\titlecontents{chapter}[.85in]
              {\singlespacing\hspace*{-.2in}}{\contentslabel{.5in}}{\hspace*{-.5in}}{\titlerule*[6pt]{.}\contentspage}[\vspace*{-5pt}]

\titlecontents{section}[1.35in]
              {\singlespacing\hspace*{-.2in}}{\contentslabel{.5in}}{\hspace*{-.5in}}{\titlerule*[6pt]{.}\contentspage}[\vspace*{-10pt}]

\titlecontents{section}[1.35in]
              {\singlespacing\hspace*{-.2in}}{\contentslabel{.5in}}{\hspace*{-.5in}}{\titlerule*[6pt]{.}\contentspage}[\vspace*{-5pt}]

\titlecontents{figure}[6.3em]
              {\singlespacing\hspace*{-6.3em}}{Figure~\thecontentslabel.~}{}{\titlerule*[6pt]{.}\contentspage}[\vspace*{-5pt}]


\titlecontents{table}[6.3em]
              {\singlespacing\hspace*{-6.3em}}{Table~\thecontentslabel.~}{}{\titlerule*[6pt]{.}\contentspage}[\vspace*{-5pt}]


\titlecontents{subsection}[1.5in + 2em]
              {\singlespacing\hspace*{-.2in}}{\contentslabel{.5in}}{\hspace*{-.5in}}{\titlerule*[6pt]{.}\contentspage}[\vspace*{5pt}]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% An algorithm environment for typesetting algorithms consistently.
%% Takes 2 arguments--the input and output.
%% Current plan is to typeset input and output only and switch font to
              %% rm
\newlength{\@alginputwidth}
\setlength{\@alginputwidth}{\textwidth}
\newlength{\@alglspace}
\settowidth{\@alglspace}{\rm Output:}
\addtolength{\@alginputwidth}{-2\@alglspace}
\newenvironment{uncgalgorithm}[2]{%
%Stuff before
\mbox{}
\rm
\begin{itemize}
\item[\rlap{Input:}] \hspace{\@alglspace} \parbox[t]{\@alginputwidth}{#1}
\item
  [\rlap{Output:}] \hspace{\@alglspace} \parbox[t]{\@alginputwidth}{#2}
\end{itemize}
}
{
%Stuff after
\relax
}

% shrink the distance between equations in multiline environments
%\setlength\jot{-1ex}
\renewcommand*\arraystretch{0.75}
\raggedbottom

%\providecommand*{\toclevel@shortchapter}{0}
%\providecommand*{\toclevel@section}{1}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  APPENDIX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\renewcommand\appendix{\par
  \setcounter{chapter}{0}%
  \setcounter{section}{0}%
  \renewcommand\thechapter{\@Alph\c@chapter}

\titleformat{\chapter}[display]{\centering}{APPENDIX\ \thechapter}{0pt}
            {\uppercase
} % before-code
\titlespacing{\chapter}{0pt}{-24pt}{12pt}
\titlecontents{chapter}[102pt]
              {\addvspace{10pt}\singlespacing\hspace*{-102pt}}{APPENDIX~\thecontentslabel.~}{}{\titlerule*[6pt]{.}\contentspage}[\vspace*{-10pt}]
}
