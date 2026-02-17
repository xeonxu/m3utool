ROS_BIN := $(HOME)/.roswell/bin
export PATH := $(ROS_BIN):$(PATH)

LISP ?= ros run

ifeq ($(OS),Windows_NT)
    # Windows: use ros without qlot
    RUN_CMD = 
    ENV_INSTALL_CMD = @echo "Windows detected: Skipping qlot install. Please ensure dependencies are in local-projects."; \
	rm -rf ~/.roswell/local-projects/cl-excel; \
	git clone https://github.com/gwangjinkim/cl-excel ~/.roswell/local-projects/cl-excel; \
	ros -e '(ql:register-local-projects)'
else
    # Linux/macOS: use qlot environment
    RUN_CMD = qlot exec
    ENV_INSTALL_CMD = @if ! command -v qlot > /dev/null; then \
		echo "Qlot not found. Installing..."; \
		ros -e '(ql:update-dist "quicklisp" :prompt nil)'; \
		ros install fukamachi/qlot; \
	else \
		echo "Qlot is already installed."; \
	fi; \
	echo "Installing project dependencies..."; \
	qlot install
endif

.PHONY: install build ros-build clean help

## Build binary
build: install
	$(RUN_CMD) $(LISP) \
		--load m3utool.asd \
		--eval '(ql:quickload :deploy)' \
		--eval '(deploy:define-library deploy::compression-lib :dont-deploy t)' \
		--eval '(ql:quickload :m3utool)' \
		--eval '(asdf:make :m3utool)' \
		--eval '(quit)'

## Build binary with ros
ros-build: install
	$(RUN_CMD) ros dump executable m3utool.ros -o m3utool

install:
	@echo "Checking environment..."
	@if ! command -v ros > /dev/null; then \
		echo "Roswell not found. Installing..."; \
		if [ -x "$$(command -v apt-get)" ]; then \
			sudo apt-get update && sudo apt-get install -y curl make automake autoconf gcc bzip2 git; \
		fi; \
		curl -L https://raw.githubusercontent.com/roswell/roswell/release/scripts/install-for-ci.sh | CI=true sh; \
	else \
		echo "Roswell is already installed."; \
	fi

	$(ENV_INSTALL_CMD)

clean:
	rm -rf m3utool m3utool.exe roswell .qlot/ bin/

help:  ## Display callable targets
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
