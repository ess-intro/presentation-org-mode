# a little error checking
GNUMAKEFLAGS += --warn-undefined-variables

ARTEFACTS = ./artefacts

MAINORG = ess-org.org
BEAMERORG = ess-org-beamer.org

DOTANGLE = ./dotangle.el

CSS = ${ARTEFACTS}/floattoc.css
ELORGEL = ${ARTEFACTS}/el-org.el
TANGLED = ${ELORGEL} ${CSS}

PUBLISHED = ${ARTEFACTS}/ess-org.html \
				${ARTEFACTS}/ess-org.pdf \
				${ARTEFACTS}/ess-org-beamer.pdf


# these are the files we want on github (other than our normal
# "source" files)
ARTEFACTSFILES = ${PUBLISHED} ${CSS}

# set up to allow evaluating R source blocks
EMACSLL = (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (R . t)))
# trust evaulations in *this* context -- a file we create CAUTION!!!
EMACSTRUSTEVAL = (setq org-confirm-babel-evaluate nil)
# load our elisp code, set up for evaluation
EMACSSETUP = (progn (package-initialize) (load-file \"${ELORGEL}\") ${EMACSLL} ${EMACSTRUSTEVAL})
# now, evaluate all org source blocks in the file
EMACSORGBLOCKS = (do-org-blocks)


all: tangled published

${TANGLED}: ${MAINORG}
	${DOTANGLE} $<

tangled: ${TANGLED}

${PUBLISHED}: published

published: ${MAINORG} ${TANGLED}
	emacs --file ${MAINORG} \
		--eval "${EMACSSETUP}" \
		--eval "${EMACSORGBLOCKS}" \
		--eval "(org-publish-project \"ess-org\")" \
		--batch

gitci: ${ARTEFACTSFILES}
	for af in ${ARTEFACTSFILES}; do \
		git diff --exit-code --quiet $${af} || git commit -m "Built by makefile" $${af}; \
	done
