#!/usr/bin/env bash

source ./env

curl --request POST --url https://api.telegram.org/bot${TELEGRAM_TOKEN}/setWebhook --header 'content-type: application/json' --data '{"url": "https://97zpndo0vk.execute-api.eu-central-1.amazonaws.com/telegram/bot'${TELEGRAM_TOKEN}'"}'
