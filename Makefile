ROS_BIN := /C/Users/$(USER)/.roswell/lisp/quicklisp/bin:$(HOME)/.roswell/lisp/quicklisp/bin:$(HOME)/.roswell/bin
export PATH := $(ROS_BIN):$(PATH)

LISP ?= ros run
RUN_CMD ?= qlot exec

ifeq ($(OS),Windows_NT)
    # qlot without parallel and symbol link functions which windows doesn't have.
    QLOT_SRC=djhaskin987/qlot
else
    QLOT_SRC=fukamachi/qlot
endif

.PHONY: prepare build ros-build test clean help

build: prepare test ## Build binary
	$(RUN_CMD) $(LISP) \
            --load m3utool.asd \
            --eval '(ql:quickload :m3utool)' \
            --eval '(asdf:make :m3utool)' \
            --eval '(quit)'

ros-build: prepare test ## Build binary with ros
ifeq ($(OS),Windows_NT)
	$(RUN_CMD) ros dump executable m3utool.ros -o m3utool.exe
else
	$(RUN_CMD) ros dump executable m3utool.ros -o m3utool
endif

build-alpine: test-alpine prepare-alpine
	@echo "Building pure musl binary via native SBCL..."
	@sbcl --noinform --non-interactive \
		--eval '(ql:quickload :deploy)' \
		--load m3utool.asd \
		--eval '(ql:quickload :m3utool)' \
		--eval '(asdf:make :m3utool)' \
		--eval '(quit)'

prepare-alpine: ## Prepare environment for alpine
	@if ! command -v sbcl > /dev/null; then \
		apk add --no-cache sbcl bash curl git gcc musl-dev openssl-dev zlib-dev zstd-dev zip; \
	        curl -O https://beta.quicklisp.org/quicklisp.lisp; \
	        sbcl --noinform --non-interactive --load quicklisp.lisp --eval '(quicklisp-quickstart:install)'; \
	        echo '(load "~/quicklisp/setup.lisp")' > ~/.sbclrc; \
	fi
	@if [ ! -d "$(HOME)/quicklisp/local-projects/cl-excel" ]; then \
		mkdir -p $(HOME)/quicklisp/local-projects; \
		git clone https://github.com/gwangjinkim/cl-excel $(HOME)/quicklisp/local-projects/cl-excel; \
	fi

test-alpine: prepare-alpine ## Run tests under alpine
	@sbcl --noinform --non-interactive \
		--eval '(ql:quickload :deploy)' \
		--load m3utool.asd \
		--eval '(ql:quickload :m3utool/tests)' \
		--eval '(asdf:test-system :m3utool)' \
		--eval '(quit)'

test: prepare ## Run tests
	$(RUN_CMD) $(LISP) \
		--load m3utool.asd \
		--eval '(ql:quickload :m3utool/tests)' \
		--eval '(asdf:test-system :m3utool)' \
		--eval '(quit)'

prepare: ## Prepare environment for ubuntu, msys
	@echo "Checking environment..."
	@if ! command -v ros > /dev/null; then \
		echo "Roswell not found. Installing..."; \
		if [ -x "$$(command -v apt-get)" ]; then \
			sudo apt-get update && sudo apt-get install -y curl make automake autoconf gcc bzip2 git; \
		elif [ -x "$$(command -v pacman)" ]; then \
			pacman -Sy --noconfirm make git curl zip unzip mingw-w64-x86_64-gcc mingw-w64-x86_64-openssl; \
		fi; \
		curl -L https://raw.githubusercontent.com/roswell/roswell/release/scripts/install-for-ci.sh | CI=true sh; \
	else \
		echo "Roswell is already installed."; \
	fi; \
	if ! command -v qlot > /dev/null; then \
		echo "Qlot not found. Installing..."; \
		ros -e '(ql:update-dist "quicklisp" :prompt nil)'; \
		ros install $(QLOT_SRC); \
		ros update quicklisp; \
	else \
		echo "Qlot is already installed."; \
	fi; \
	echo "Installing project dependencies..."; \
	qlot install

clean: ## Clean targets
	rm -rf m3utool m3utool.exe roswell .qlot/ bin/

help:  ## Display callable targets
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
