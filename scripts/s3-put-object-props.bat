@echo off
set BUCKET_NAME="%%BUCKET_NAME%%"
set OBJECT_NAME="%%OBJECT_NAME%%"
set S3_ENDPOINT_URL="%%S3_ENDPOINT_URL%%"
rem See: https://awscli.amazonaws.com/v2/documentation/api/latest/reference/s3api/put-object.html
s3 put --endpoint-url=%S3_ENDPOINT_URL% %BUCKET_NAME%/%OBJECT_NAME% filename="%%INPUT_FILE%%" %%METADATA_PROPERTIES%%
