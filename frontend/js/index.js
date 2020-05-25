function ticketOnDragStart(event) {

    const ticket = event.target.id;
    const from = document.getElementById(ticket).columnId;

    event.dataTransfer.setData('ticket', ticket);
    event.dataTransfer.setData('from', from);
}

const addTicket =
  async () => {
    const boardId    = getCurrentBoard();
    const strName    = document.getElementById('input-ticket-name').value;
    const strContent = document.getElementById('input-ticket-content').value;
    await restApi.createTicket(boardId, strName, strContent);
    setVisible(panels.NONE);
  }

const updateTicket =
  async () => {
    const createTicketSection = document.getElementById('create-tickets');
    const columnId   = createTicketSection.columnId;
    const ticketId   = createTicketSection.ticketId;
    const strName    = document.getElementById('input-ticket-name').value;
    const strContent = document.getElementById('input-ticket-content').value;
    await restApi.updateTicket(columnId, ticketId, strName, strContent);

    // TODO refresh action
    //const boardId = getCurrentBoard();
    //restApi.getBoard(boardId);
  }

const cancelAddTicket =
  async () => {
    setVisible(panels.NONE);
    clearTicketPanel();
  }

const deleteTicket =
  async () => {
    const createTicketSection = document.getElementById('create-tickets');
    
    const boardId  = getCurrentBoard();
    const columnId = createTicketSection.columnId;
    const ticketId = createTicketSection.ticketId;

    await restApi.deleteTicket(boardId, columnId, ticketId);
    clearTicketPanel();
    setVisible(panels.NONE);
  }

const addUser =
  async () => {
    const strName     = document.getElementById('input-user-name').value;
    const strPassword = document.getElementById('input-user-password').value;
    await restApi.createUser(strName, strPassword);
    setVisible(panels.NONE);
  }

const loginUser =
  async () => {

    const strName     = document.getElementById('input-login-user-name').value;
    const strPassword = document.getElementById('input-login-user-password').value;
    const errSection  = document.getElementById('login-error-section');
    const errMsg      = document.getElementById('login-error-message');

    function clearLoginError() {
      errMsg.innerText = '';
      errSection.classList.remove('active');     
    }

    function setLoginError(errTxt) {
      errMsg.innerText = errTxt;
      errSection.classList.add('active');     
    }

    function loginSuccess() {
      setVisible(panels.NONE);
      clearLoginError();
      location = location;
    }

    restApi.login(strName,
                  strPassword,
                  clearLoginError,
                  loginSuccess,
                  setLoginError);
  }

function showLoginUserPanel(event) {
  clearLoginPanel();
  setVisible(panels.LOGIN);
}

const logout =
  async () => {
    await restApi.logout();
    location = location;
  }

function showNewUserPanel(event) {
  clearUserPanel();
  setVisible(panels.CREATE_USER);
}

function showNewTicketPanel(event) {
  clearTicketPanel();
  setVisible(panels.CREATE_TICKET);
}

function getBoards() {
  restApi.withBoards(buildBoards(restApi));
}

function unauthHandler() {
  console.log("Not logged in.");
  setVisible(panels.LOGIN);
}

function main() {
  getBoards();
}

const restApi = createRestApi(unauthHandler);

main();
