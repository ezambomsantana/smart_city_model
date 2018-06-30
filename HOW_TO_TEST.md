# Testando a propagação de incidentes de trânsito

A seguir segue o passo-a-passo para testar a propagação de incidentes de
trânsito. Basicamente precisamos configurar um evento na simulação, que pode
ser o fechamento de uma rua/via ou redução do seu fluxo, e enviar sinais de
aviso do incidente através do protocolo AMQP. Ao enviar essa notificação de
incidente, os carros que passarem por essas Placas de Mensagens Variáveis devem
recalcular as suas rotas para evitarem tal incidente.

Para realizar esse pequeno teste foi adicionado o diretório `events_input` ao
repositório.

## Configuração do evento

Para programar um evento de trânsito na simulação deve ser criado um arquivo
chamado `events.csv` e dentro do `config.xml` apontar para o mesmo, da seguinte
forma:

```
<scsimulator_config>
<config trip_file="../events_input/trips-with-uuid.xml" map_file="../events_input/map_reduced.xml" output_file="../output/events.xml" events_file="../events_input/events.csv" generate_graph="true" simulation_time="3000"/>
</scsimulator_config>
```

No arquivo `events.csv` temos a seguinte estrutura:

* Tipo de evento (`close_street` ou `reduce_capacity`)
* Identificador do nó do grafo da cidade onde é iniciado o evento
* Identificador do nó do grafo da cidade onde é finalizado o evento
* Tempo de início do evento na simulação
* Duração do evento na simualação
* Porcentagem de redução da capacidade da via. Caso o tipo de evento seja
  `close_street` esse campo deve ser nulo (0)

A seguir é apresentado um exemplo de configuração de evento que está presente
no arquivo `events_input/events.csv`:

```
close_street;3;5;10;2900;0
```

Com isso teremos um evento do tipo "fechar rua" que se inicia no nó 3 do grafo e
termina no nó 5 do grafo, ele está agendado para iniciar no tick 10 da
simulação e irá durar 2900 ticks. Como o evento é do tipo "fechar rua" a
porcentagem de redução da capacidade da rua é nula.


## Notificando evento de trânsito

Para notificar um evento de trânsito para os carros da simulação basta enviar
uma mensagem usando RabbitMQ no seguinte formato:

```
<nó_da placa_de_sinalização>.<nó_do_início_do_evento>.<nó_do final_do evento>
```

Essa mensagem deve ser enviada para o tópico `traffic_sign`. Para isso foi
implementado um cliente com o objetivo de apenas realizar testes no arquivo
`src/events_handler.erl`.


## Realizando teste

Para realizar esse teste temos um grafo simples no arquivo
`events_input/map_reduced.xml`. Nele temos apenas 6 nós e 6 arestas, a seguir a
lista de arestas:

* 1 -> 2
* 2 -> 3
* 3 -> 4
* 3 -> 5
* 4 -> 5
* 5 -> 6

Além disso, todos so carros na simulação saem do nó 1 e vão até o nó 6.
Portanto, o caminho mais curto para todos é 1 -> 2 -> 3 -> 5 -> 6. Agendamos
um evento do tipo "fechar rua" para a aresta 3 -> 5. Com isso, os carros aos
serem notificados devem mudar a sua rota para passar pelas arestas 3 -> 4 -> 5,
e não apenas 3 -> 5. Essa notificação será enviada através do cliente RabbitMQ
mencionado anteriormente.

### Passo-a-passo

Para a realização do teste uma instância do servidor RabbitMQ deve estar sendo
executada na sua própria máquina (`localhost`).

1 - Abra dois terminais (ambos dentro do diretório `src`)

2 - Em um dos terminais compile o módulo `events_handler.erl`

```
$ erlc events_handler.erl
```

3 - Nesse mesmo terminal abra o console do Erlang e carregue o módulo

```
$ erl
$ c(events_handler).
```

4 - No outro terminal inicie a simulação

```
$ make smart_city_run CMD_LINE_OPT="--batch"
```

5 - Quando decidir dispara a notificação de exemplo durante a simulação execute
    o seguinte comando no terminal erlang que está aberto:

```
$ events_handler:test().
```

A notificação do evento deverá ser tratada pelo simulador e o caminho dos
carros que ali passarem deve ser modificado.
