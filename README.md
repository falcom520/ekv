# ekv

通过Mochiweb框架按照Restful风格的K/V存储

主要是学习Erlang和Mochiweb框架


Deploy and start

make
./start-dev.sh



使用方法:

存储数据:
http://localhost:8080/api/add

curl -H "user_id: 00001" -H "secure_id: 1111" -X PUT http://localhost:8080/api/add -d "k=k003&v=v003"


读取数据:
http://localhost:8080/api/read

curl -H "user_id: 00001" -H "secure_id: 1111"  http://localhost:8080/api/read?k=k001


删除数据:
http://localhost:8080/api/delete

curl -H "user_id: 00001" -H "secure_id: 1111"  -X DELETE http://localhost:8080/api/delete?k=k003
