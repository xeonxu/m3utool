ROS_BIN := /C/Users/$(USER)/.roswell/lisp/quicklisp/bin:$(HOME)/.roswell/lisp/quicklisp/bin:$(HOME)/.roswell/bin:/root/.roswell/bin
export PATH := $(ROS_BIN):$(PATH)

ROS_CMD ?= ros

LISP ?= $(ROS_CMD) run
CMD_PREFIX ?= qlot exec

# Retrieving the latest Git Tag as the version string. Appending '-dirty' at the end of the version string, if there is uncommit codes exist in workspace.
VERSION := $(shell git describe --tags --always --dirty 2>/dev/null || echo "unknown")

ifeq ($(OS),Windows_NT)
    # qlot without parallel and symbol link functions which windows doesn't have.
    QLOT_SRC=djhaskin987/qlot
else
    QLOT_SRC=fukamachi/qlot
endif

.PHONY: prepare build ros-build test clean help version

build: prepare test version ## Build binary
	$(CMD_PREFIX) $(LISP) \
            --load m3utool.asd \
            --eval '(ql:quickload :m3utool)' \
            --eval '(asdf:make :m3utool)' \
            --eval '(quit)'

ros-build: prepare test version ## Build binary with ros
ifeq ($(OS),Windows_NT)
	@$(CMD_PREFIX) $(ROS_CMD) dump executable m3utool.ros -o m3utool.exe
else
	@$(CMD_PREFIX) $(ROS_CMD) dump executable m3utool.ros -o m3utool
endif

test: prepare ## Run tests
	@$(CMD_PREFIX) $(LISP) \
		--load m3utool.asd \
		--eval '(ql:quickload :m3utool/tests)' \
		--eval '(asdf:test-system :m3utool)' \
		--eval '(quit)'

# To build roswell from source under alpine, these packages are needed: automake autoconf. Meanwhile, it have to hack install-for-ci.sh to comment out the libcurl4-openssl-dev-lib line since there's no package named this, instead it should be openssl-dev on alpine.
prepare: ## Prepare environment for ubuntu, msys
	@echo "Checking environment..."
	@if ! command -v ros > /dev/null; then \
		echo "Roswell not found. Installing..."; \
		if [ -x "$$(command -v apt-get)" ]; then \
			sudo apt-get update && sudo apt-get install -y curl make automake autoconf gcc bzip2 git; \
		elif [ -x "$$(command -v pacman)" ]; then \
			pacman -Sy --noconfirm make git curl zip unzip mingw-w64-x86_64-gcc mingw-w64-x86_64-openssl; \
		elif [ -x "$$(command -v apk)" ]; then \
			apk update && apk add --no-cache bash curl git gcc musl-dev openssl-dev zlib-dev zstd-dev zip roswell; \
		fi; \
		if [ ! -x "$$(command -v apk)" ]; then \
			curl -L https://raw.githubusercontent.com/roswell/roswell/release/scripts/install-for-ci.sh | CI=true sh; \
		fi; \
	else \
		echo "Roswell is already installed."; \
	fi;
# Don't append '-L' parameter to 'ros' here even under alpine, since we have to init the ros itself firstly.
	@if ! command -v qlot > /dev/null; then \
		echo "Qlot not found. Installing..."; \
		ros -e '(ql:update-dist "quicklisp" :prompt nil)'; \
		ros install $(QLOT_SRC); \
		ros update quicklisp; \
	else \
		echo "Qlot is already installed."; \
	fi;
	@echo "Installing project dependencies...";
	@qlot install


clean: ## Clean targets
	rm -rf m3utool m3utool.exe roswell .qlot/ bin/

help:  ## Display callable targets
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

# --- Target to generate the version.txt file ---
version:
	@echo "Generating version.txt with tag: $(VERSION)"
	@echo "$(VERSION)" > version.txt
