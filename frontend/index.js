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
    moveTicket(getCurrentBoard(), fromId, toId, ticketId);
}

function getCurrentBoard() {
  const queryString = window.location.search;
  const urlParams = new URLSearchParams(queryString);
  return urlParams.get('board');
}

function ticketSelect(event) {

  const ticketId      = event.target.id;
  const ticketElement = document.getElementById(ticketId);
  const columnId      = ticketElement.columnId;

  fetchTicket(columnId, ticketId,
    function(ticket) {
      document.getElementById('inp_name').value = ticket.name;
      document.getElementById('inp_content').value = ticket.content;
      document.getElementById('create-tickets').classList.add('visible');
    });
  
}

function showCreate(event) {
  document.getElementById('inp_name').value = '';
  document.getElementById('inp_content').value = '';
  document.getElementById('create-tickets').classList.add('visible');
}

function cancelTicketAdd(event) {
  document.getElementById('inp_name').value = '';
  document.getElementById('inp_content').value = '';
  document.getElementById('create-tickets').classList.remove('visible');
}

function update(event) {
}

function create(event) {
  const boardName  = getCurrentBoard();
  const strName    = document.getElementById('inp_name').value;
  const strContent = document.getElementById('inp_content').value;
  createTicket(boardName, strName, strContent);
  document.getElementById('create-tickets').classList.remove('visible');
}

function main() {
  const boardName = getCurrentBoard();
  fetchBoard(boardName);
}

main();