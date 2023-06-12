(ns trabalho-unifor.core
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def transactions (atom {}))

(defn get-b3-stocks-data []
  (let [response (client/get "http://brapi.dev/api/quote/list"
                             {:query-params {"sortBy" "name"
                                             "sortOrder" "asc"}})]
    (if (= 200 (:status response))
      (let [stocks (-> (:body response)
                       (json/parse-string true)
                       :stocks)]
        (doall (map (fn [stock]
                      (println (str "Nome: " (:name stock) ", Código: " (:stock stock))))
                    stocks)))
      )))

(defn get-stock-data [symbol]
  (let [response (client/get (str "https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=" symbol ".SAO&apikey=SE7A5J3E5HU6VA5U"))
        data (-> response :body json/parse-string)]
    (if (= 200 (:status response))
      (let [quote (get data "Global Quote")]
        {:symbol (get quote "01. symbol")
         :open (get quote "02. open")
         :high (get quote "03. high")
         :low (get quote "04. low")
         :price (get quote "05. price")
         :volume (get quote "06. volume")
         :latest-trading-day (get quote "07. latest trading day")
         :previous-close (get quote "08. previous close")
         :change (get quote "09. change")
         :change-percent (get quote "10. change percent")})
      (throw (Exception. "Falha ao buscar dados da ação.")))))

(defn buy-stock [symbol price quantity]
  (swap! transactions update symbol (fn [current]
                                      (let [new-quantity (if (nil? current) quantity (+ quantity (:total current)))]
                                        {:total new-quantity
                                         :transactions (conj (if (nil? current) [] (:transactions current))
                                                             {:type :buy :price price :quantity quantity})}))))

(defn sell-stock [symbol price quantity]
  (let [current (get @transactions symbol)]
    (if (>= (:total current) quantity)
      (let [new-quantity (- (:total current) quantity)]
        (swap! transactions assoc symbol
               {:total new-quantity
                :transactions (conj (:transactions current) {:type :sell :price price :quantity quantity})}))
      (println "Você não possui ações suficientes para vender."))))

(defn ask-user-for-symbol []
  (println "Digite o código da ação:")
  (read-line))

(defn ask-user-for-quantity [action]
  (println (str "Digite a quantidade de ações que deseja " action ":"))
  (Integer/parseInt (read-line)))

(defn ask-user-for-price [action]
  (println (str "Digite o preço para " action " a ação:"))
  (Double/parseDouble (read-line)))

(defn ask-user-for-action [symbol price]
  (println (str "Você quer comprar a ação " symbol " pelo preço de " price " (s/n)?"))
  (let [response (read-line)]
    (when (= response "s")
      (let [quantity (ask-user-for-quantity "comprar")]
        (buy-stock symbol price quantity)))))

(defn ask-user-for-sale [symbol]
  (println (str "Você quer vender a ação " symbol " (s/n)?"))
  (let [response (read-line)]
    (when (= response "s")
      (let [quantity (ask-user-for-quantity "vender")
            price (ask-user-for-price "vender")]
        (sell-stock symbol price quantity)))))

(defn print-transactions []
  (println "Extrato das transações:")
  (doseq [[symbol {transactions :transactions total :total}] @transactions]
    (println (str "Ações: " symbol ", Total: " total))
    (doseq [transaction transactions]
      (println (str "Tipo: " (:type transaction)
                    ", Preço: " (:price transaction)
                    ", Quantidade: " (:quantity transaction)
                    ", Valor Total: " (* (:quantity transaction) (Double/parseDouble (str (:price transaction)))))))))

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Ações B3:")
  (get-b3-stocks-data)
  (let [symbol (ask-user-for-symbol)
        stock-data (get-stock-data symbol)]
    (println (str "Nome da ação: " (get stock-data :symbol)
                  "\nPreço de abertura: " (get stock-data :open)
                  "\nPreço máximo: " (get stock-data :high)
                  "\nPreço mínimo: " (get stock-data :low)
                  "\nÚltimo preço: " (get stock-data :price)
                  "\nVolume: " (get stock-data :volume)
                  "\nData da última negociação: " (get stock-data :latest-trading-day)
                  "\nPreço de fechamento do dia anterior: " (get stock-data :previous-close)
                  "\nVariação do dia (em R$): " (get stock-data :change)
                  "\nVariação do dia (percentual): " (get stock-data :change-percent)
                  "\nTipo de ativo: Não disponível"
                  "\nDescrição da ação: Não disponível"))
    (ask-user-for-action symbol (get stock-data :price))
    (ask-user-for-sale symbol)
    (print-transactions)))
