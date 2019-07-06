FROM nginx
RUN mkdir -p /var/log/nginx/ && touch /var/log/nginx/error.log && touch /var/log/nginx/access.log && chmod -R 777 /var/log/nginx/
COPY html /usr/share/nginx/html/
COPY nginx /etc/nginx/conf.d/
CMD /bin/bash -c "envsubst < /etc/nginx/conf.d/mysite.template > /etc/nginx/conf.d/default.conf && exec nginx -g 'daemon off;'"
