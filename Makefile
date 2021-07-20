SANDBOX_DIR?=sandbox


# prepare emacs sandbox with local library and dependencies installed
sandbox:
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) --install-deps -vv

# run emacs in sandbox
.PHONY: emacs
emacs: sandbox
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) interactive

test: sandbox
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) batch -vv -- -l tests/postrace-test.el --eval "(postrace-test-suite)"


clean:
	rm -rf $(SANDBOX_DIR)
