# saz-echo-bot
This application is essential when you want to run an echo bot, which simply sends back user messages a choosen number of times. It is supposed to be run on a server. Echo bot is implemented for two messengers: Telegram and Slack. Note that for Telegram you should first create a bot through BotFather, for more information see [telegram.org](https://core.telegram.org/bots). And for Slack you should create a Slack App, then create a bot user and install it to a workspace, see [slack.com](https://api.slack.com/bot-users) to find out more.
## :gear: Functionality
### :grey_question: Help messages
A user can send a command which will print a configurable help message. For Telegram the command is `</help>`, and it is `<_help>` for Slack.
### :repeat: Choosing a number of repeats
A user can send a command which will print a configurable repeat message. In Telegram on `</repeat>` command he will see a buttonpad with numbers from 1 to 5. In Slack on  `<_repeat>` command he can react to repeat message through :one:, :two:, :three:, :four:, :five: emojis.
## :ledger: Logging
Supported levels of logging: DEBUG, INFO, WARN, ERROR, the default log file is `<./log/bot.log>`. All sent messages and repeat number changes are logged on debug level. Supported exceptions are: response exceptions (occur when response status from Telegram/Slack servers was not ok), parsing exceptions (occur when JSON structures in responses from Telegram/Slack servers are unexpected), bad callback data exceptions (only applicable to Telegram and occur only when some source code of keyboard formation is changed).
## :wrench: Configurability
Almost all functionality is configurable through file `<./config/bot.local>`. The supported items are: bot tokens, help message, repeat message, deafault number of user message repeats, logging configuration. For convenience there is `<./config/bot.template>` file in the repository, that could be used for reference. 
## :computer: App using guide
### Downloading and building
Clone or dowload from [this](https://github.com/stanislav-az/echo-bot.git) repository. To build an executable use `<stack build>` command. Note that you have to choose which bot to run in the configuration file.
### Configuration files preparation
It is recommended to use repeat_msg parameters provided in `<./bot.config>` file. The token and channel parameters are required to change. The tokens can be obtained in the process of creating bots (see links in header paragraph). To obtain slack channel number see a link to a direct message conversation with your bot, or use Slack API [tester](https://api.slack.com/methods/im.list). 
### Running tests
To test project use `<stack test>` command.
### Launching
To launch app use `<stack exec saz-echo-bot-exe>` command.