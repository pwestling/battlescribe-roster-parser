gcloud config configurations activate personal
gcloud compute ssh pwestlingg@multi-use-docker --zone us-east1-b --project oneoff-project --command="cd /home/pwestl && bash ./deploy-bs2tts-backend.sh"
