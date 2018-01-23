# deploy.sh
#! /bin/bash

SHA1=$(git rev-parse --short HEAD)

APPLICATION=$1
APPLICATION_NAME=$1
if [ "$APPLICATION" == "Webserver" ]
then
	APPLICATION_NAME="Backend"
fi
ENVIRON=$2

docker build --rm=false -t heroku:$SHA1-$APPLICATION-$ENVIRON -f Dockerfile.${APPLICATION,,} .
docker tag heroku:$SHA1-$APPLICATION-$ENVIRON registry.heroku.com/howl-server/webserver


#mkdir ~/.aws/
#touch ~/.aws/credentials
#printf "aws_access_key_id = %s\naws_secret_access_key = %s\n" "$AWS_ACCESS_KEY_ID" "$AWS_SECRET_ACCESS_KEY" >> ~/.aws/credentials
#touch ~/.aws/config
#printf "region=eu-central-1 \n output=json" >> ~/.aws/config

# Push image to ECR
# $(aws ecr get-login --region eu-central-1)
# docker push $AWS_ACCOUNT_ID.dkr.ecr.eu-central-1.amazonaws.com/howl-docker-repo:$SHA1-$APPLICATION-$ENVIRON

# # Create new Elastic Beanstalk version
# EB_BUCKET=howl-deploy-bucket
# DOCKERRUN_FILE=$SHA1-$APPLICATION-Dockerrun.aws.json
# sed "s/<TAG>/$SHA1-$APPLICATION-$ENVIRON/" < Dockerrun.aws.json.template > Dockerrun.aws.json
# zip -r $DOCKERRUN_FILE.zip .ebextensions Dockerrun.aws.json
# aws s3 cp $DOCKERRUN_FILE.zip s3://$EB_BUCKET/$DOCKERRUN_FILE.zip --region eu-central-1
# aws elasticbeanstalk create-application-version --application-name Howl-$APPLICATION_NAME \
#     --version-label $SHA1-$APPLICATION-$ENVIRON --source-bundle S3Bucket=$EB_BUCKET,S3Key=$DOCKERRUN_FILE.zip \
#     --region eu-central-1

# # Update Elastic Beanstalk environment to new version
# aws elasticbeanstalk update-environment --environment-name Howl-$APPLICATION-$ENVIRON \
#     --version-label $SHA1-$APPLICATION-$ENVIRON \
#     --region eu-central-1
