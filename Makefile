# there's a famous comment in the V6 (i think) Unix kernel sources
# that says something like "you are not expected to understand the
# following line".  here, it's not "understanding", but "the ability
# to run" that is in question -- the make process here may or may not
# be reproducible on someone else's machine.  good luck -- there be
# unicorns!

# a little error checking
GNUMAKEFLAGS += --warn-undefined-variables

# where the bits we produce go
ARTEFACTSDIR = ./artefacts
# org file tangles here on the way to artefacts; we only copy over if
# there is a difference
TANGLEDDIR = ./tangled
# to keep make from making over and over again, we touch(1) some
# otherwise unused files in this directory
TOUCHEDDIR = ./touched

# our main .org file
MAINORG = ess-org.org
# well, this is the beamer presentation, and might arguably be
# considered our main work product
BEAMERORG = ess-org-beamer.org
# and, our demo file
DEMOORG = ess-org-demo.org

ORGFILES = ${MAINORG} ${BEAMERORG} ${DEMOORG}

# script to tangle org files
DOTANGLE = ./dotangle.el

ELORGEL = ${ARTEFACTSDIR}/el-org.el
TWBSCSS = ${ARTEFACTSDIR}/ggm-twbs.css
TWBSJS = ${ARTEFACTSDIR}/ggm-twbs.js
HTMLFRAG = ${ARTEFACTSDIR}/ggm-twbs.html-fragment

# ess-org-startup.org: built during make process, and converts the
# contents of our .css file into a #+HTML_HEAD for inclusiong in the
# .html file https://stackoverflow.com/a/56407596/1527747
ESSORGSTARTUPORG = ${ARTEFACTSDIR}/ess-org-startup.org

DEMOEXPANDEDORG = ${ARTEFACTSDIR}/ess-org-demo-expanded.org
DEMORESULTSORG = ${ARTEFACTSDIR}/ess-org-demo-results.org

TANGLEDFILES = ${ELORGEL} ${HTMLFRAG} ${TWBSCSS} ${TWBSJS}
DERIVEDFILES = ${DEMOEXPANDEDORG} \
					${DEMORESULTSORG} \
					${ESSORGSTARTUPORG} \
					${CSSFILES}

PUBLISHEDHTMLFILES = \
				${ARTEFACTSDIR}/ess-org-demo-results.html \
				${ARTEFACTSDIR}/ess-org-beamer.html
PUBLISHEDPDFFILES = \
				${ARTEFACTSDIR}/ess-org-demo-results.pdf \
				${ARTEFACTSDIR}/ess-org-beamer.pdf
PUBLISHEDFILES = ${PUBLISHEDHTMLFILES} ${PUBLISHEDPDFFILES}


TOUCHEDFILES = ${TOUCHEDDIR}/tangle ${TOUCHEDDIR}/publish

# these are generated from an emacs non-batch session.  we don't
# normally re-create these.  but, if they need to be re-created, they
# will following a 'make clean'
CSSFILES = $(subst .html,.css,${PUBLISHEDHTMLFILES})

EXARTEFACTSSED = "s|^\#+SETUPFILE: ./artefacts/|\#+SETUPFILE: ./|"

# these are the files we want on github (other than our normal
# "source" files)
ARTEFACTSFILES = ${PUBLISHEDFILES} ${HTMLFRAG} \
			${DEMOEXPANDEDORG} ${DEMORESULTSORG} \
			${CSSFILES}

# set up to allow evaluating R source blocks
EMACSLL = (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (org . t) (python . t) (R . t)))
# trust evaulations in *this* context -- a file we create CAUTION!!!
EMACSTRUSTEVAL = (setq org-confirm-babel-evaluate nil)
# we need ess, htmlize in the load path
EMACSLOADPATH = (setq load-path (append (list "." "./artefacts" "~/.emacs.d/straight/build/ess" "~/.emacs.d/straight/build/htmlize") load-path))
# and, load ess-autoloads
EMACSGETESSAUTOLOADS =  "(require 'ess-autoloads)"
# load our elisp code, set up for evaluation
EMACSSETUP = "(progn ${EMACSLL} (require 'el-org) ${EMACSTRUSTEVAL})"
# now, evaluate all org source blocks in the file
EMACSORGBLOCKS = (resultify-org-blocks)
# now, evaluate all *non* org source blocks in the file
EMACSNONORGBLOCKS = (let ((ess-ask-for-ess-directory nil)) (resultify-non-org-blocks) (save-buffer))
# 
EMACSORGIFY = (progn (orgify-all-non-org-blocks) (save-buffer))
# output to stdout the CSS used for htlmize'ing our files.  this needs
# to be done in "full emacs" mode (not --batch, etc.), since htmlize's
# behaviour is dependent on what the platform it is running on.
EMACSCSSIFY = "(let ((htmlize-hyperlink-style \"\")) (org-html-htmlize-generate-css) (append-to-file (point-min) (point-max) \"/dev/stdout\"))"

all: tangle publish

# tangle to ./tangled
${TANGLEDFILES}: tangle

tangle: ${TOUCHEDDIR}/tangle

${TOUCHEDDIR}/tangle: ${ORGFILES}
	${DOTANGLE} $<
	for longname in ${TANGLEDFILES}; do \
		i=`basename $${longname}`; \
		cmp -s ${TANGLEDDIR}/$$i ${ARTEFACTSDIR}/$$i || \
			cp -p ${TANGLEDDIR}/$$i ${ARTEFACTSDIR}/$$i; \
	done
	touch $@

${ARTEFACTSDIR}/ess-org.html: publish

${ARTEFACTSDIR}/ess-org.pdf: publish

${ARTEFACTSDIR}/ess-org-beamer.pdf: publish

publish: ${TOUCHEDDIR}/publish

# we turn off org mode publish's time stamps, since apparently, if the
# time stamp is good, the file isn't re-created, even if it doesn't
# exist.
${TOUCHEDDIR}/publish: ${ORGFILES} \
							${TANGLEDFILES} essorgstartuporg \
							${DEMORESULTSORG} \
							${CSSFILES}
	emacs --file ${MAINORG} \
		--eval '${EMACSLOADPATH}' \
		--eval ${EMACSSETUP} \
		--eval "${EMACSORGBLOCKS}" \
		--eval "(setq org-html-htmlize-output-type 'css)" \
		--eval '(setq org-publish-use-timestamps-flag nil)' \
		--eval "(progn (org-publish-project \"ess-org\"))" \
		--batch
	touch $@

# admittedly, this is a bit silly.  but...  so, i write macros for
# keystrokes, to have consistency, etc.  but, in the file users open
# in emacs, i want the macros expanded.  so, i do this.  this has the
# advantage of keeping everything "user facing" in ./artefacts (but,
# still.)
${DEMOEXPANDEDORG}: ${DEMOORG} tangle
	cat $< | sed ${EXARTEFACTSSED} > $@
	emacs --file $@ \
		--eval "(org-macro-replace-all (org-macro--collect-macros))" \
		--eval "(save-buffer)" \
		--batch

${DEMORESULTSORG}: ${DEMOEXPANDEDORG} tangle
	cat $< | sed ${EXARTEFACTSSED} > $@
	emacs --file $@ \
		--eval '${EMACSLOADPATH}' \
		--eval ${EMACSSETUP} \
		--eval ${EMACSGETESSAUTOLOADS} \
		--eval "${EMACSNONORGBLOCKS}" \
		--eval "${EMACSORGIFY}" \
		--batch

artefacts/%.css: | artefacts/%.org tangle
	emacs --file $< \
		--eval ${EMACSCSSIFY} \
	    --eval "(progn (kill-buffer) (kill-emacs))" > $@

artefacts/%.css: | %.org tangle
	emacs --file $< \
		--eval ${EMACSCSSIFY} \
	    --eval "(progn (kill-buffer) (kill-emacs))" > $@


HEADERTXT = \#+HTML_HEAD: 
HEADER = echo -n "${HEADERTXT} "
${ESSORGSTARTUPORG}: ${HTMLFRAG}
	( \
		(cat $< | sed 's/^/${HEADERTXT} /;s/  */ /g'); \
	) > $@

essorgstartuporg: ${ESSORGSTARTUPORG}

gitci: ${ARTEFACTSFILES}
	for af in ${ARTEFACTSFILES}; do \
		git diff --exit-code --quiet $${af} || git commit -m "Built by makefile" $${af}; \
	done

release: gitci gitcleanp
	git push

gitcleanp:
	@git status --short | awk '$$1 !~ /[?][?]/ { x++ } END {exit x}' || \
			(echo "ERROR: git working tree not clean" > /dev/stderr; exit 1)

clean: 
	rm -f ${TANGLEDFILES} ${PUBLISHEDFILES} ${DERIVEDFILES}
	rm -f ${TOUCHEDFILES}
