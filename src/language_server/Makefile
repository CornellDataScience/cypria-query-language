build:
	sudo su cds; cd ..; make build; cd language_server

deploy: 
	authbind --deep pm2 start backend.js

stop: 
	authbind --deep pm2 stop backend.js

restart: 
	authbind --deep pm2 restart backend.js
	
