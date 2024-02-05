PROJECT = luproc
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

BUILD_DEPS += relx

DEPS += luerl
DEPS += jsx

ERLC_OPTS += +debug_info

SHELL_OPTS += -eval 'application:ensure_all_started($(PROJECT)).'

include luerl_lib_template.mk
include erlang.mk

autopatch-luerl::
	@cp -r autopatches/luerl $(DEPS_DIR)/luerl/autopatches
	@for file in `find $(DEPS_DIR)/luerl/src/ -type f -name '*.erl'`; do \
		line=`sed -n '/^-module.*/=' $$file`; \
		sed $$line's|^\(.*\)$$|\1\n-include("autopatches/luerl.autopatch").|g' -i "$$file"; \
	done;