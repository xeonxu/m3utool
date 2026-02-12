LISP ?= sbcl

build:  ## Build binary
	$(LISP) --eval '(ql:quickload :deploy)' \
		--load m3utool.asd \
		--eval '(ql:quickload :m3utool)' \
		--eval '(asdf:make :m3utool)' \
		--eval '(quit)'

ros-build:  ## Build binary with ros
	ros dump executable m3utool.ros -o m3utool

clean:
	rm -rf bin m3utool

help:  ## Display callable targets
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
