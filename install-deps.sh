#!/bin/bash

# This script will install the dependencies needed by the InterSCSimulator.

DEPENDENCIES_DIR='deps'
ROOT_DIR=`pwd`
RABBITMQ_URL='https://www.rabbitmq.com/releases/rabbitmq-erlang-client'
RABBITMQ_VERSION='3.6.11'

if [[ ! -e $DEPENDENCIES_DIR ]]; then
	mkdir -p $DEPENDENCIES_DIR
	cd $DEPENDENCIES_DIR
else
	echo "You already have the _build directory, if you want to reinstall it,"
	echo "please, remove it before run this script!"
	exit -5
fi

wget "$RABBITMQ_URL/v$RABBITMQ_VERSION/rabbit_common-$RABBITMQ_VERSION.ez" 2> /dev/null
wget "$RABBITMQ_URL/v$RABBITMQ_VERSION/amqp_client-$RABBITMQ_VERSION.ez" 2> /dev/null

if [[ $? != 0 ]]; then
	echo "Error during the download of .ez files!"
	exit -1
else
	echo "I: .ez files were downloaded"
fi

unzip -q "rabbit_common-$RABBITMQ_VERSION.ez"
unzip -q "amqp_client-$RABBITMQ_VERSION.ez"

if [[ $? != 0 ]]; then
	echo "Error during the descompress of .ez files!"
	exit -2
else
	echo "I: .ez files were descompressed"
fi

ln -s "amqp_client-$RABBITMQ_VERSION" amqp_client
cd amqp_client/include
ln -s "../../rabbit_common-$RABBITMQ_VERSION" rabbit_common

if [[ $? != 0 ]]; then
	echo "Error during the linkage!"
	exit -3
else
	echo "I: dependencies was linked"
fi

cd $ROOT_DIR
