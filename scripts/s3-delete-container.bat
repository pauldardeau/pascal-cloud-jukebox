@echo off
set BUCKET_NAME="%%BUCKET_NAME%%"
set S3_ENDPOINT_URL="%%S3_ENDPOINT_URL%%"
aws s3 rb --endpoint-url=%S3_ENDPOINT_URL% s3://%BUCKET_NAME%
