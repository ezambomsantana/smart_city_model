#!/bin/bash

# This script will install the dependencies needed by the InterSCSimulator.

DEPENDENCIES_DIR='deps'
ROOT_DIR=`pwd`
RABBITMQ_URL='https://github.com/rabbitmq/rabbitmq-server/releases/download'
RABBITMQ_VERSION_UNDERSCORE='3_6_11'
RABBITMQ_VERSION_DOT='3.6.11'

if [[ ! -e $DEPENDENCIES_DIR ]]; then
	mkdir -p $DEPENDENCIES_DIR
	cd $DEPENDENCIES_DIR
else
	echo "You already have the _build directory, if you want to reinstall it,"
	echo "please, remove it before run this script!"
	exit -0
fi

wget "$RABBITMQ_URL/rabbitmq_v$RABBITMQ_VERSION_UNDERSCORE/rabbit_common-$RABBITMQ_VERSION_DOT.ez"
wget "$RABBITMQ_URL/rabbitmq_v$RABBITMQ_VERSION_UNDERSCORE/amqp_client-$RABBITMQ_VERSION_DOT.ez"

if [[ $? != 0 ]]; then
	echo "Error during the download of .ez files!"
	exit -1
else
	echo "I: .ez files were downloaded"
fi

unzip -q "rabbit_common-$RABBITMQ_VERSION_DOT.ez"
unzip -q "amqp_client-$RABBITMQ_VERSION_DOT.ez"

if [[ $? != 0 ]]; then
	echo "Error during the descompress of .ez files!"
	exit -2
else
	echo "I: .ez files were descompressed"
fi

ln -s "amqp_client-$RABBITMQ_VERSION_DOT" amqp_client
cd amqp_client/include
ln -s "../../rabbit_common-$RABBITMQ_VERSION_DOT" rabbit_common

if [[ $? != 0 ]]; then
	echo "Error during the linkage!"
	exit -3
else
	echo "I: dependencies was linked"
fi

cd $ROOT_DIR
