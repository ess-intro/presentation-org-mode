# a little error checking
GNUMAKEFLAGS += --warn-undefined-variables

BUILT = ./built

MAINORG = ess-org.org
BEAMERORG = ess-org-beamer.org

DOTANGLE = ./dotangle.el

ELORGEL = ${BUILT}/el-org.el
TANGLED = ${ELORGEL}

PUBLISHED = ${BUILT}/ess-org.html ${BUILT}/ess-org.pdf ${BUILT}/ess-org-beamer.pdf

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

${PUBLISHED}: ${MAINORG} ${TANGLED}
	emacs --file ${MAINORG} \
		--eval "${EMACSSETUP}" \
		--eval "${EMACSORGBLOCKS}" \
		--eval "(org-publish-all)" \
		--batch

published: ${PUBLISHED}
