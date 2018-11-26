hostname="raw.githubusercontent.com"
port=443
trust_cert_file_location=`curl-config --ca`

sudo bash -c "echo -n | openssl s_client -showcerts -connect $hostname:$port \
    2>/dev/null  | sed -ne '/-BEGIN CERTIFICATE-/,/-END CERTIFICATE-/p'  \
    >> $trust_cert_file_location"
