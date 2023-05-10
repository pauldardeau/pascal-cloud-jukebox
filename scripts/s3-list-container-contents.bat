@echo off
set S3_ENDPOINT_URL="%%S3_ENDPOINT_URL%%"
set BUCKET_NAME="%%BUCKET_NAME%%"
aws s3 ls s3://%BUCKET_NAME% --endpoint-url=%S3_ENDPOINT_URL% 
