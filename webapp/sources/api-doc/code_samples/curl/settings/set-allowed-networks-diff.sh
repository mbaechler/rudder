curl --header "X-API-Token: yourToken" --request POST https://rudder.example.com/rudder/api/latest/settings/allowed_networks/root/diff --data '{"allowed_networks": { "delete": ["192.168.1.0/24"], "add": ["192.168.2.0/24"] } }'
