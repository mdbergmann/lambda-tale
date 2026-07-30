# Lambda's Tale — a Bard's Tale-like dungeon-crawler ENGINE for cl-amiga.
# Self-contained repo: uses the clamiga binary built in the cl-amiga
# repo (../amigasources/cl-amiga).  Games live in sibling repos
# (e.g. ../closure-tale) and load the engine via src/load.lisp.
#
#   make test    Run the engine's Lisp test suite (host clamiga)
#   make assets  Regenerate the data/gfx wall-piece ILBMs (tools/gen-walls.lisp)
#   make pack    Build a tile pack from one facade picture:
#                  make pack ART=art/house.iff OUT=data/gfx-town/
#   make preview Composite a pack into a picture you can look at:
#                  make preview PACK=data/gfx-town/ [OUT=preview.iff]
#
# Override the interpreter with CLAMIGA=/path/to/clamiga.

# When this engine is checked out as a submodule of a game repo, the
# cl-amiga submodule sits beside it (../cl-amiga); in a standalone
# checkout the development clone is the fallback.
CLAMIGA ?= $(firstword $(wildcard ../cl-amiga/build/host/clamiga ../amigasources/cl-amiga/build/host/clamiga) ../amigasources/cl-amiga/build/host/clamiga)
HEAP    ?= 16M
OUT     ?=

.PHONY: test assets pack preview clamiga-check install-hooks

# The suite counts failures, but a top-level form that SIGNALS is only
# reported by the loader and skipped — its checks never run, and the
# summary stays clean.  So the gate reads the output too: any ERROR line
# fails the run, whatever the failure count says.
test: clamiga-check
	@out=$$($(CLAMIGA) --heap $(HEAP) --non-interactive \
	          --load tests/run-tests.lisp 2>&1); \
	 status=$$?; \
	 printf '%s\n' "$$out"; \
	 if printf '%s\n' "$$out" | grep -q '^ERROR'; then \
	   echo "=> a top-level form signalled: its checks never ran"; \
	   exit 1; \
	 fi; \
	 exit $$status

assets: clamiga-check
	$(CLAMIGA) --heap $(HEAP) --non-interactive --load tools/make-assets.lisp

# ART is the flat, front-on wall picture (any size, any depth); OUT the
# pack directory, defaulting to the active profile's own.  A world with
# more to say than one image writes its own script instead — see
# ../closure-tale/worlds/closure/gfx/make-pack.lisp.
pack: clamiga-check
	@test -n "$(ART)" || { \
	  echo "usage: make pack ART=<facade.iff> [OUT=<dir/>]"; exit 1; }
	$(CLAMIGA) --heap $(HEAP) --non-interactive \
	  --load src/load.lisp \
	  --load tools/gen-walls.lisp \
	  --load tools/gen-pack-from-art.lisp \
	  --eval '(tale::generate-pack-from-art "$(ART)" :out $(if $(OUT),"$(OUT)",nil))'

preview: clamiga-check
	@test -n "$(PACK)" || { \
	  echo "usage: make preview PACK=<dir/> [OUT=<file.iff>]"; exit 1; }
	$(CLAMIGA) --heap $(HEAP) --non-interactive \
	  --load src/load.lisp \
	  --load tools/preview-view.lisp \
	  --eval '(tale::preview-pack "$(PACK)" "$(if $(OUT),$(OUT),preview.iff)")'
	@echo "wrote $(if $(OUT),$(OUT),preview.iff)"

clamiga-check:
	@test -x $(CLAMIGA) || { \
	  echo "clamiga not found at $(CLAMIGA)."; \
	  echo "Build it first: make host in the cl-amiga checkout (or set CLAMIGA=/path/to/clamiga)"; \
	  exit 1; }

install-hooks:
	@git config core.hooksPath githooks
	@chmod +x githooks/* scripts/review/pre-commit.sh 2>/dev/null || true
	@echo "=> auto-review hook activated (core.hooksPath=githooks)"
	@echo "   bypass one commit with 'git commit --no-verify'; disable with CLAUDE_AUTO_REVIEW=0"
