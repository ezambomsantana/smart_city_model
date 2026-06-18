MOCK_SIMULATORS_TOP = ../..

# Target to install RabbitMQ dependencies
install-deps:
	@echo "Installing RabbitMQ dependencies..."
	@cd .. && ./install-deps.sh

# Make the main target depend on dependencies installation
all: install-deps

include $(MOCK_SIMULATORS_TOP)/GNUmakesettings.inc
