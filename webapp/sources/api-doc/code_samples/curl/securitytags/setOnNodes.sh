curl --header "X-API-Token: yourToken" --request POST --header "Content-Type: application/json" 'https://rudder.example.com/rudder/api/latest/securitytags/nodes' --data '[{"tenantId":"zone1","nodeIds":["node1","node2"]},{"tenantId":"zone2", "nodeIds":["node3"]}]'
