$ErrorActionPreference = "Stop"
Set-PSDebug -Trace 1

# Windows Remote Management — Ansible Documentation https://docs.ansible.com/ansible/latest/user_guide/windows_winrm.html
# AnsibleをWindows上のWSLにインストール - ソフトウェアエンジニアリング - Torutk http://www.torutk.com/projects/swe/wiki/Ansible%E3%82%92Windows%E4%B8%8A%E3%81%AEWSL%E3%81%AB%E3%82%A4%E3%83%B3%E3%82%B9%E3%83%88%E3%83%BC%E3%83%AB

Set-ExecutionPolicy RemoteSigned

# Invoke-RestMethod、Invoke-WebRequestが失敗する https://social.technet.microsoft.com/Forums/ja-JP/77a771fe-69c4-4d61-b87f-b2915cc31102/invokerestmethod12289invokewebrequest1236422833259431237712427?forum=powershellja
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls11
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
Invoke-WebRequest https://github.com/ansible/ansible/raw/devel/examples/scripts/ConfigureRemotingForAnsible.ps1 -OutFile ConfigureRemotingForAnsible.ps1
Set-PSDebug -Off
.\ConfigureRemotingForAnsible.ps1
Set-PSDebug -Trace 1

Get-NetConnectionProfile -Name "識別されていないネットワーク" | Set-NetConnectionProfile -NetworkCategory private
winrm set winrm/config/service '@{AllowUnencrypted="true"}'
winrm quickconfig

Set-PSDebug -Off