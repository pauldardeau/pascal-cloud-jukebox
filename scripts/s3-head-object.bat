@echo off
set BUCKET_NAME="%%BUCKET_NAME%%"
set OBJECT_NAME="%%OBJECT_NAME%%"
set S3_ENDPOINT_URL="%%S3_ENDPOINT_URL%%"

aws s3api head-object --endpoint-url=%S3_ENDPOINT_URL% --bucket %BUCKET_NAME% --key %OBJECT_NAME%
