# a little error checking
GNUMAKEFLAGS += --warn-undefined-variables

BUILT = ./built

MAINORG = ess-org.org
BEAMERORG = ess-org-beamer.org

DOTANGLE = ./dotangle.el

ELORGEL = ${BUILT}/el-org.el
TANGLED = ${ELORGEL}

ESSORGHTML = ${BUILT}/ess-org.html
ESSORGPDF = ${BUILT}/ess-org.pdf
ESSORGBEAMERPDF = ${BUILT}/ess-org-beamer.pdf

EXPORTED = ${ESSORGHTML} ${ESSORGPDF} ${ESSORGBEAMERPDF}

# set up to allow evaluating R source blocks
EMACSLL = (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (R . t)))
# trust evaulations in *this* context -- a file we create CAUTION!!!
EMACSTRUSTEVAL = (setq org-confirm-babel-evaluate nil)
# load our elisp code, set up for evaluation
EMACSSETUP = (progn (package-initialize) (load-file \"${ELORGEL}\") ${EMACSLL} ${EMACSTRUSTEVAL})
# now, evaluate all org source blocks in the file
EMACSORGBLOCKS = (do-org-blocks)


all: ${TANGLED} ${EXPORTED}

${TANGLED}: ${MAINORG}
	${DOTANGLE} $<

tangled: ${TANGLED}

${ESSORGHTML}: ${MAINORG}
	emacs --file ${MAINORG} \
		--eval "${EMACSSETUP}" \
		--eval "${EMACSORGBLOCKS}" \
		--eval "(org-html-export-to-html)" \
		--batch

${ESSORGPDF}: ${MAINORG}
	emacs --file ${MAINORG} \
		--eval "${EMACSSETUP}" \
		--eval "${EMACSORGBLOCKS}" \
		--eval "(org-latex-export-to-pdf)" \
		--batch

${ESSORGBEAMERPDF}: ${BEAMERORG}
	emacs --file ${BEAMERORG} \
		--eval "${EMACSSETUP}" \
		--eval "${EMACSORGBLOCKS}" \
		--eval "(org-beamer-export-to-pdf)" \
		--batch
