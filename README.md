# 一个简单的反向代理

## 运行方式
`r-proxy [port]`。配置文件为当前目录下conf.yaml。默认监听端口是3000。


## 配置文件格式
```yaml
rules:
    - regex: http://www.domain.com/path/
      url: http://www.domain.com
      ip: 127.0.0.1
      port: 8080

    - regex: http://www.domain.com/path/(.*)
      url: http://www.domain.com/$1
      ip: 127.0.0.1
      port: 8080

    - regex: http://www.domain.com/sockjs-node/.*
      ip: 127.0.0.1
      port: 8080
```

每个规则包含1个必须的属性`regex`和3个可选的属性`url`, `host`, `ip`, `port`。当请求的url匹配`regex`时，请求被重定向到`url`, `host`, `ip`, `port`所定义的资源上。

例如:
```yaml
regex: http://www.baidu.com/(.*)
url: http://www.google.com/?query=$1
host: www.bing.com
ip: 127.0.0.1
port: 8080
```
会截获访问百度的请求，并连接`127.0.0.1:8080`后，发送请求
```
GET /?query=xxxx
Host: www.bing.com
// 其他原始请求数据
```
并将响应结果返回客户端。
