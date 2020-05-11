function ticketOnDragStart(event) {

    const ticket = event.target.id;
    const from = document.getElementById(ticket).columnId;

    event.dataTransfer.setData('ticket', ticket);
    event.dataTransfer.setData('from', from);
}

function columnHeaderAllowDrop(event) {
    event.preventDefault();
}

function columnHeaderDropTicket(event) {
    event.preventDefault();
    const fromId   = event.dataTransfer.getData('from')
    const toId     = event.target.id;  
    const ticketId = event.dataTransfer.getData('ticket')
    clearTicketPanel();
    setVisible(panels.NONE);
    moveTicket(getCurrentBoard(), fromId, toId, ticketId);
}

const addTicket =
  async () => {
    const boardName  = getCurrentBoard();
    const strName    = document.getElementById('input-ticket-name').value;
    const strContent = document.getElementById('input-ticket-content').value;
    await createTicket(boardName, strName, strContent);
    setVisible(panels.NONE);
  }

const updateTicket =
  async () => {
    const createTicketSection = document.getElementById('create-tickets');
    const columnId   = createTicketSection.columnId;
    const ticketId   = createTicketSection.ticketId;
    const strName    = document.getElementById('input-ticket-name').value;
    const strContent = document.getElementById('input-ticket-content').value;
    await restUpdateTicket(columnId, ticketId, strName, strContent);

    const boardName = getCurrentBoard();
    fetchBoard(boardName);
  }

const cancelAddTicket =
  async () => {
    setVisible(panels.NONE);
    clearTicketPanel();
  }

const deleteTicket =
  async () => {
    const createTicketSection = document.getElementById('create-tickets');
    
    const boardName = getCurrentBoard();
    const columnId  = createTicketSection.columnId;
    const ticketId  = createTicketSection.ticketId;

    await restDeleteTicket(boardName, columnId, ticketId);
    clearTicketPanel();
    setVisible(panels.NONE);
  }

const addUser =
  async () => {
    const strName     = document.getElementById('input-user-name').value;
    const strPassword = document.getElementById('input-user-password').value;
    await createUser(strName, strPassword);
    setVisible(panels.NONE);
  }

const loginUser =
  async () => {
    const strName     = document.getElementById('input-login-user-name').value;
    const strPassword = document.getElementById('input-login-user-password').value;
    const response    = await login(strName, strPassword);

    switch(response.status) {

      case 401:
        break;

      case 200:
        setVisible(panels.NONE);
        break;
    }
  }

function showLoginUserPanel(event) {
  clearLoginPanel();
  setVisible(panels.LOGIN);
}

const doLogout =
  async () => {
    await logout();
  }

function showNewUserPanel(event) {
  clearUserPanel();
  setVisible(panels.CREATE_USER);
}

function showNewTicketPanel(event) {
  clearTicketPanel();
  setVisible(panels.CREATE_TICKET);
}

function main() {
  const boardName = getCurrentBoard();
  fetchBoard(boardName, showLoginUserPanel);
}

main();
