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

# script to tangle org files
DOTANGLE = ./dotangle.el

CSS = ${ARTEFACTSDIR}/floattoc.css
ELORGEL = ${ARTEFACTSDIR}/el-org.el

DEMOORG = ess-org-demo.org
DEMORESULTSORG = ess-org-demo-results.org

TANGLEDFILES = ${ELORGEL} ${CSS}
DERIVEDFILES = ${DEMORESULTSORG}
PUBLISHEDFILES = \
				${ARTEFACTSDIR}/ess-org.html \
				${ARTEFACTSDIR}/ess-org.pdf \
				${ARTEFACTSDIR}/ess-org-demo-results.html \
				${ARTEFACTSDIR}/ess-org-demo-results.pdf \
				${ARTEFACTSDIR}/ess-org-beamer.html \
				${ARTEFACTSDIR}/ess-org-beamer.pdf


# these are the files we want on github (other than our normal
# "source" files)
ARTEFACTSFILES = ${PUBLISHEDFILES} ${CSS}

# set up to allow evaluating R source blocks
EMACSLL = (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (org . t) (python . t) (R . t)))
# trust evaulations in *this* context -- a file we create CAUTION!!!
EMACSTRUSTEVAL = (setq org-confirm-babel-evaluate nil)
# we need ess, htmlize in the load path
EMACSLOADPATH = (setq load-path (append (list "~/.emacs.d/straight/build/ess" "~/.emacs.d/straight/build/htmlize") load-path))
# and, load ess-autoloads
EMACSGETESSAUTOLOADS =  "(require 'ess-autoloads)"
# load our elisp code, set up for evaluation
EMACSSETUP = (progn (load-file \"${ELORGEL}\") ${EMACSLL} ${EMACSTRUSTEVAL})
# now, evaluate all org source blocks in the file
EMACSORGBLOCKS = (evaluate-org-blocks)
# now, evaluate all *non* org source blocks in the file
EMACSNONORGBLOCKS = (progn (evaluate-non-org-blocks) (save-buffer))
# 
EMACSORGIFY = (progn (orgify-all-non-org-blocks) (save-buffer))

# ess-org-startup.org: built during make process, and converts the
# contents of our .css file into a #+HTML_HEAD for inclusiong in the
# .html file https://stackoverflow.com/a/56407596/1527747
ESSORGSTARTUPORG = ${ARTEFACTSDIR}/ess-org-startup.org


all: tangle publish

# tangle to ./tangled
${TANGLEDFILES}: tangle

tangle: ${TOUCHEDDIR}/tangle

${TOUCHEDDIR}/tangle: ${MAINORG}
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

# when i try to do this with --batch, i don't get happiness
${TOUCHEDDIR}/publish: ${MAINORG} ${TANGLEDFILES} ess-org-demo-results.org essorgstartuporg
	emacs --file ${MAINORG} \
		--eval "${EMACSSETUP}" \
		--eval "${EMACSORGBLOCKS}" \
		--eval '${EMACSLOADPATH}' \
		--eval "(org-publish-project \"ess-org\")" \
	    --eval "(progn (kill-buffer) (kill-emacs))"
	touch $@

${DEMORESULTSORG}: ${DEMOORG} tangle
	cp -p $< $@
	emacs --file $@ \
		--eval "${EMACSSETUP}" \
		--eval '${EMACSLOADPATH}' \
		--eval ${EMACSGETESSAUTOLOADS} \
		--eval "${EMACSNONORGBLOCKS}" \
		--eval "${EMACSORGIFY}" \
		--batch

HEADERTXT = \#+HTML_HEAD: 
HEADER = echo -n "${HEADERTXT} "
${ESSORGSTARTUPORG}: ${CSS}
	( \
		${HEADER}; echo '<style type="text/css">'; \
		(cat $< | sed 's/^/${HEADERTXT} /;s/  */ /g'); \
		${HEADER}; echo ' </style>' \
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
