# deploy.sh
#! /bin/bash

SHA1=$1

mkdir ~/.aws/
touch ~/.aws/credentials
printf "aws_access_key_id = %s\naws_secret_access_key = %s\n" "$AWS_ACCESS_KEY_ID" "$AWS_SECRET_ACCESS_KEY" >> ~/.aws/credentials
touch ~/.aws/config
printf "region=eu-central-1\noutput=json" >> ~/.aws/config

# Push image to ECR
$(aws ecr get-login --region eu-central-1)
docker push <account-id>.dkr.ecr.eu-central-1.amazonaws.com/howl-docker-repo:$SHA1

# Create new Elastic Beanstalk version
EB_BUCKET=howl-deploy-bucket
DOCKERRUN_FILE=$SHA1-Dockerrun.aws.json
sed "s/<TAG>/$SHA1/" < Dockerrun.aws.json.template > $DOCKERRUN_FILE
aws s3 cp $DOCKERRUN_FILE s3://$EB_BUCKET/$DOCKERRUN_FILE --region eu-central-1
aws elasticbeanstalk create-application-version --application-name howl \
    --version-label $SHA1 --source-bundle S3Bucket=$EB_BUCKET,S3Key=$DOCKERRUN_FILE \
    --region eu-central-1

# Update Elastic Beanstalk environment to new version
aws elasticbeanstalk update-environment --environment-name howl-env \
    --version-label $SHA1 \
    --region eu-central-1
