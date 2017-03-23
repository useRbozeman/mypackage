% This template was adapted from texMemo.cls (Rob Oakes, Copyright 2010) and cscrs.sty (Michael Lerch, 2015).


% texMemo.cls information:
% This document class provides a simple memo for LaTeX users.
%  It is based on article.cls and inherits most of the functionality that class.
% Author: Rob Oakes, Copyright 2010.  Released under the LGPL, version 3.
% A copy of the LGPL can be found at http://www.gnu.org/licenses/lgpl.html

% cscrs.sty information:
% author: Michael Lerch
% date: 2015/02/07 v 1.0
% description: A style file for Montana State University CSCRS

% cscrs_notes.cls information:
% author: Andrea Mack
% date: 09/16/2016
% description: This adapted tempate is for MSU SCRS meeting notes.

% ------------------------------------------------------------------------------------------------------------------------- %

% ------------------------------------------------------------------------------------------------------------------------- %

%note: the color files were copied from the cscrs.sty file, but do not appear to be working


\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{cscrs_notes}[10/1/2016 SCRS notes class]
\RequirePackage{palatino}
\usepackage{graphicx}
\usepackage{xcolor}
\usepackage{fancyhdr}

\definecolor{cscrsColor1}{HTML}{000000} % MSU blue
\definecolor{cscrsColor2}{HTML}{000000} % MSU yellow
\definecolor{cscrsColor3}{HTML}{000000} % MSU gold
\definecolor{cscrsColor4}{HTML}{000000} % MSU red
\definecolor{cscrsColor5}{HTML}{000000} % MSU green
\definecolor{cscrsColor6}{HTML}{000000} % MSU light blue
\definecolor{cscrsColor7}{HTML}{000000} % MSU light yellow
\definecolor{cscrsColor8}{HTML}{000000} % MSU rich black
\definecolor{cscrsColor9}{HTML}{000000} % MSU murky blue
\definecolor{cscrsColor10}{HTML}{000000} % MSU grayish
\definecolor{cscrsColor11}{HTML}{000000} % MSU purple
\definecolor{cscrsColor12}{HTML}{000000} % MSU grayish 2



    \definecolor{cscrsColor1}{HTML}{013E7D} % MSU blue
 \definecolor{cscrsColor2}{HTML}{F0AD00} % MSU yellow
    \definecolor{cscrsColor3}{HTML}{B79257} % MSU gold
    \definecolor{cscrsColor4}{HTML}{F15A38} % MSU red
    \definecolor{cscrsColor5}{HTML}{C2D251} % MSU green
    \definecolor{cscrsColor6}{HTML}{3C9CD7} % MSU light blue
    \definecolor{cscrsColor7}{HTML}{F7EF6F} % MSU light yellow
    \definecolor{cscrsColor8}{HTML}{070004} % MSU rich black
    \definecolor{cscrsColor9}{HTML}{25353E} % MSU murky blue
    \definecolor{cscrsColor10}{HTML}{4D3E3B} % MSU grayish
    \definecolor{cscrsColor11}{HTML}{56344E} % MSU purple
    \definecolor{cscrsColor12}{HTML}{817C80} % MSU grayish 2


\ProcessOptions\relax

% page setup
\special{papersize=8.5in,11in}
\textheight=21.5cm
\textwidth=14.95cm
\topmargin=-5.5mm
\oddsidemargin=8mm
\evensidemargin=8mm

% uses article base class options
\LoadClassWithOptions{article}

% similar to usepackage{}
\RequirePackage{ifthen}

% define new commands

\def\@clientname{\relax}
\newcommand{\clientname}[1]{\gdef\@clientname{#1}}

\def\@clientloc{\relax}
\newcommand{\clientloc}[1]{\gdef\@clientloc{#1}}

\def\@mtgdate{\relax}
\newcommand{\mtgdate}[1]{\gdef\@mtgdate{#1}}

\def\@leadconsultant{\relax}
\newcommand{\leadconsultant}[1]{\gdef\@leadconsultant{#1}}

\def\@secdconsultant{\relax}
\newcommand{\secdconsultant}[1]{\gdef\@secdconsultant{#1}}

\def\@studytitle{\relax}
\newcommand{\studytitle}[1]{\gdef\@studytitle{#1}}

\def\@director{\relax}
\newcommand{\director}[1]{\gdef\@director{#1}}


% formatting
\newcommand\decorativeline[1][1pt]{
	\par\noindent%
	\rule[0.5ex]{\linewidth}{#1}\par
}

% \newcommand\line002[4][t]{%
 %  \par\smallskip\noindent\parbox[#1]{.333\textwidth}{\raggedright\texttt{+}#2}%
  %  \parbox[#1]{.333\textwidth}{\raggedleft\texttt{#4}}\par\smallskip%
% }

% margins
\RequirePackage{geometry}
\geometry{margin=1.0in}

% headers and footers 
\pagestyle{fancy}
\fancyhf{}

\renewcommand{\footrulewidth}{0.4pt}
\renewcommand{\headrulewidth}{0.4pt}

\fancyfoot[C]{\color{cscrsColor3}\thepage}
\fancyfoot[R]{\setlength{\unitlength}{1mm}
\begin{picture}(0,0)
\put(-40,-40){\includegraphics[width=6cm]{logo}}
\end{picture}}
\fancyfoot[L]{\color{cscrsColor3}MSU SCRS}


% titles

\newcommand\makenotestitle{
		{\begin{center}
	\LARGE\bf
            {\color{cscrsColor1}\large \bfseries Montana State University}

            {\color{cscrsColor1}\large \bfseries Statistical Consulting and Research Services}
	\textsc{\ifthenelse{\equal{\@studytitle}{\relax}}{}{\item [{ }] \@studytitle}}

 \vskip 1.5em

	\end{center}}

	%Info

		%\ifthenelse{\equal{\@clientname}{\relax}}{}{ \textbf{Client Name:} \@clientname}

\noindent\begin{minipage}[b]{0.35\hsize}
  \raggedright
\ifthenelse{\equal{\@clientname}{\relax}}{}{ \textbf{Client Name:} \@clientname}
 \end{minipage}
\begin{minipage}[b]{0.7\hsize}
  \raggedleft
\ifthenelse{\equal{\@leadconsultant}{\relax}}{}{ \textbf{Lead Consultant:} \@leadconsultant}
\end{minipage}		
\noindent\begin{minipage}[b]{0.35\hsize}
  \raggedright
\ifthenelse{\equal{\@clientloc}{\relax}}{}{ \textbf{Client Location:} \@clientloc}
 \end{minipage}
\noindent\begin{minipage}[b]{0.7\hsize}
  \raggedleft
\ifthenelse{\equal{\@secdconsultant}{\relax}}{}{ \textbf{Secondary Consultant:} \@secdconsultant}
\end{minipage}
\noindent\begin{minipage}[b]{0.35\hsize}
  \raggedright
\ifthenelse{\equal{\@mtgdate}{\relax}}{}{ \textbf{Meeting Date:} \@mtgdate}
\end{minipage}
\hfill
\begin{minipage}[b]{0.7\hsize}
  \raggedleft
\ifthenelse{\equal{\@director}{\relax}}{}{ \textbf{Director:} \@director}
\end{minipage}
		
					



	\decorativeline\bigskip{}
}

%-------------------------------------------------------------------------------------------------------- %

