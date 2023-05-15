@echo off
set BUCKET_NAME="pjd-p-artist-songs"
set OBJECT_NAME="The-Police--Outlandos-dAmour--Masoko-Tanga.mp3"
set S3_ENDPOINT_URL="https://s3.us-central-1.wasabisys.com"

aws s3 cp --endpoint-url=%S3_ENDPOINT_URL% s3://%BUCKET_NAME%/%OBJECT_NAME% Z:\Documents\GitHub\pascal-cloud-jukebox\song-play\The-Police--Outlandos-dAmour--Masoko-Tanga.mp3 1>stdout.txt 2>stderr.txt
