@echo off
set BUCKET_NAME="%%BUCKET_NAME%%"
set OBJECT_NAME="%%OBJECT_NAME%%"
set S3_ENDPOINT_URL="%%S3_ENDPOINT_URL%%"

aws s3 cp --endpoint-url=%S3_ENDPOINT_URL% s3://%BUCKET_NAME%/%OBJECT_NAME% %%OUTPUT_FILE%% 1>stdout.txt 2>stderr.txt
