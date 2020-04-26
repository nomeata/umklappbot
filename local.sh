#!/usr/bin/env bash

source ./env

curl --request POST --url https://api.telegram.org/bot${TELEGRAM_TOKEN}/deleteWebhook --header 'content-type: application/json' --data '{}'
