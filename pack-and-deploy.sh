# deploy.sh
#! /bin/bash

SHA1=$(git rev-parse --short HEAD)

if [ $1 = "webserver" ]
then
	APPLICATION=Howl-Backend
	ENVIRONMENT=Sample-env
fi
if [ $1 = "notifications" ]
then
	APPLICATION=Howl-Notifications
	ENVIRONMENT=LowCost-env
fi

docker build --rm=false -t $AWS_ACCOUNT_ID.dkr.ecr.$AWS_DEFAULT_REGION.amazonaws.com/$AWS_REPO_NAME:$SHA1-$1 -f Dockerfile.$1.deploy .


mkdir ~/.aws/
touch ~/.aws/credentials
printf "aws_access_key_id = %s\naws_secret_access_key = %s\n" "$AWS_ACCESS_KEY_ID" "$AWS_SECRET_ACCESS_KEY" >> ~/.aws/credentials
touch ~/.aws/config
printf "region=eu-central-1\noutput=json" >> ~/.aws/config

# Push image to ECR
$(aws ecr get-login --region eu-central-1)
docker push $AWS_ACCOUNT_ID.dkr.ecr.eu-central-1.amazonaws.com/howl-docker-repo:$SHA1-$1

# Create new Elastic Beanstalk version
EB_BUCKET=howl-deploy-bucket
DOCKERRUN_FILE=$SHA1-$1-Dockerrun.aws.json
sed "s/<TAG>/$SHA1-$1/" < Dockerrun.aws.json.template > $DOCKERRUN_FILE
aws s3 cp $DOCKERRUN_FILE s3://$EB_BUCKET/$DOCKERRUN_FILE --region eu-central-1
aws elasticbeanstalk create-application-version --application-name $APPLICATION \
    --version-label $SHA1-$1 --source-bundle S3Bucket=$EB_BUCKET,S3Key=$DOCKERRUN_FILE \
    --region eu-central-1

# Update Elastic Beanstalk environment to new version
aws elasticbeanstalk update-environment --environment-name $ENVIRONMENT \
    --version-label $SHA1-$1 \
    --region eu-central-1
