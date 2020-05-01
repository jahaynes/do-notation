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
    hideCreateTicket();
    moveTicket(getCurrentBoard(), fromId, toId, ticketId);
}

function getCurrentBoard() {
  const queryString = window.location.search;
  const urlParams = new URLSearchParams(queryString);
  return urlParams.get('board');
}

function hideCreateTicket() {
  document.getElementById('create-tickets').classList.remove('visible');
}

function ticketSelect(event) {

  const ticketId      = event.target.id;
  const ticketElement = document.getElementById(ticketId);
  const columnId      = ticketElement.columnId;

  fetchTicket(columnId, ticketId,
    function(ticket) {
      document.getElementById('inp_name').value = ticket.name;
      document.getElementById('inp_content').value = ticket.content;
      const createTicketSection = document.getElementById('create-tickets');
      createTicketSection.columnId = columnId;
      createTicketSection.ticketId = ticketId;
      createTicketSection.classList.add('visible');
    });
}

function showCreate(event) {
  clearTicketPanel();
  document.getElementById('create-tickets').classList.add('visible');
}

function clearTicketPanel() {
  const createTicketSection = document.getElementById('create-tickets');
  createTicketSection.columnId = null;
  createTicketSection.ticketId = null;
  document.getElementById('inp_name').value = '';
  document.getElementById('inp_content').value = '';
}

function cancelTicketAdd(event) {
  clearTicketPanel();
  hideCreateTicket();
}

const deleteTicket =
  async (event) => {
    const createTicketSection = document.getElementById('create-tickets');
    
    const boardName = getCurrentBoard();
    const columnId  = createTicketSection.columnId;
    const ticketId  = createTicketSection.ticketId;

    await restDeleteTicket(boardName, columnId, ticketId);
    clearTicketPanel();
    hideCreateTicket();
  }

function update(event) {
}

const create =
  async (event) => {
    const boardName  = getCurrentBoard();
    const strName    = document.getElementById('inp_name').value;
    const strContent = document.getElementById('inp_content').value;
    await createTicket(boardName, strName, strContent);
    hideCreateTicket();
  }

function main() {
  const boardName = getCurrentBoard();
  fetchBoard(boardName);
}

main();
