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
TANGLEDFILES = ${ELORGEL} ${CSS}

PUBLISHEDFILES = ${ARTEFACTSDIR}/ess-org.html \
				${ARTEFACTSDIR}/ess-org.pdf \
				${ARTEFACTSDIR}/ess-org-beamer.pdf


# these are the files we want on github (other than our normal
# "source" files)
ARTEFACTSFILES = ${PUBLISHEDFILES} ${CSS}

# set up to allow evaluating R source blocks
EMACSLL = (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (R . t)))
# trust evaulations in *this* context -- a file we create CAUTION!!!
EMACSTRUSTEVAL = (setq org-confirm-babel-evaluate nil)
# load our elisp code, set up for evaluation
EMACSSETUP = (progn (package-initialize) (load-file \"${ELORGEL}\") ${EMACSLL} ${EMACSTRUSTEVAL})
# now, evaluate all org source blocks in the file
EMACSORGBLOCKS = (do-org-blocks)


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

${TOUCHEDDIR}/publish: ${MAINORG} ${TANGLEDFILES}
	emacs --file ${MAINORG} \
		--eval "${EMACSSETUP}" \
		--eval "${EMACSORGBLOCKS}" \
		--eval "(org-publish-project \"ess-org\")" \
		--batch
	touch $@

gitci: ${ARTEFACTSFILES}
	for af in ${ARTEFACTSFILES}; do \
		git diff --exit-code --quiet $${af} || git commit -m "Built by makefile" $${af}; \
	done
