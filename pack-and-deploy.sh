# deploy.sh
#! /bin/bash

SHA1=$(git rev-parse --short HEAD)

#Webserver
docker build --rm=false -t $AWS_ACCOUNT_ID.dkr.ecr.$AWS_DEFAULT_REGION.amazonaws.com/$AWS_REPO_NAME:$SHA1-webserver -f Dockerfile.deploy .

#Notifications
docker build --rm=false -t $AWS_ACCOUNT_ID.dkr.ecr.$AWS_DEFAULT_REGION.amazonaws.com/$AWS_REPO_NAME:$SHA1-notifications -f Dockerfile.notifications.deploy .


mkdir ~/.aws/
touch ~/.aws/credentials
printf "aws_access_key_id = %s\naws_secret_access_key = %s\n" "$AWS_ACCESS_KEY_ID" "$AWS_SECRET_ACCESS_KEY" >> ~/.aws/credentials
touch ~/.aws/config
printf "region=eu-central-1\noutput=json" >> ~/.aws/config

# Push image to ECR
$(aws ecr get-login --region eu-central-1)
docker push $AWS_ACCOUNT_ID.dkr.ecr.eu-central-1.amazonaws.com/howl-docker-repo:$SHA1-webserver

docker push $AWS_ACCOUNT_ID.dkr.ecr.eu-central-1.amazonaws.com/howl-docker-repo:$SHA1-notifications

# Create new Elastic Beanstalk version
#Webserver
EB_BUCKET=howl-deploy-bucket
DOCKERRUN_FILE=$SHA1-webserver-Dockerrun.aws.json
sed "s/<TAG>/$SHA1-webserver/" < Dockerrun.aws.json.template > $DOCKERRUN_FILE
aws s3 cp $DOCKERRUN_FILE s3://$EB_BUCKET/$DOCKERRUN_FILE --region eu-central-1
aws elasticbeanstalk create-application-version --application-name Howl-Backend \
    --version-label $SHA1-webserver --source-bundle S3Bucket=$EB_BUCKET,S3Key=$DOCKERRUN_FILE \
    --region eu-central-1

#Notifications
EB_BUCKET=howl-deploy-bucket
DOCKERRUN_FILE=$SHA1-notifications-Dockerrun.aws.json
sed "s/<TAG>/$SHA1-notifications/" < Dockerrun.aws.json.template > $DOCKERRUN_FILE
aws s3 cp $DOCKERRUN_FILE s3://$EB_BUCKET/$DOCKERRUN_FILE --region eu-central-1
aws elasticbeanstalk create-application-version --application-name Howl-Notifications \
    --version-label $SHA1-notifications --source-bundle S3Bucket=$EB_BUCKET,S3Key=$DOCKERRUN_FILE \
    --region eu-central-1

# Update Elastic Beanstalk environment to new version
#Webserver
aws elasticbeanstalk update-environment --environment-name Sample-env \
    --version-label $SHA1-webserver \
    --region eu-central-1

#Notifications
aws elasticbeanstalk update-environment --environment-name LowCost-env \
    --version-label $SHA1-notifications \
    --region eu-central-1
